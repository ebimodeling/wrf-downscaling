



module error_function




implicit none
private
save


public :: erf, erfc, erfcx

interface erf
   module procedure erf_r4
   module procedure derf
end interface

interface erfc
   module procedure erfc_r4
   module procedure derfc
end interface

interface erfcx
   module procedure erfcx_r4
   module procedure derfcx
end interface


integer, parameter :: r4 = selected_real_kind(6)  
integer, parameter :: r8 = selected_real_kind(12) 

contains

































































































































SUBROUTINE CALERF_r8(ARG, RESULT, JINT)

   
   
   
   integer, parameter :: rk = r8

   
   real(rk), intent(in)  :: arg
   integer,  intent(in)  :: jint
   real(rk), intent(out) :: result

   
   INTEGER :: I

   real(rk) :: X, Y, YSQ, XNUM, XDEN, DEL

   
   
   
   real(rk), parameter :: ZERO   = 0.0E0_rk
   real(rk), parameter :: FOUR   = 4.0E0_rk
   real(rk), parameter :: ONE    = 1.0E0_rk
   real(rk), parameter :: HALF   = 0.5E0_rk
   real(rk), parameter :: TWO    = 2.0E0_rk
   real(rk), parameter :: SQRPI  = 5.6418958354775628695E-1_rk
   real(rk), parameter :: THRESH = 0.46875E0_rk
   real(rk), parameter :: SIXTEN = 16.0E0_rk











   
   
   
   real(rk), parameter :: XINF   =   1.79E308_r8
   real(rk), parameter :: XNEG   = -26.628E0_r8
   real(rk), parameter :: XSMALL =   1.11E-16_r8
   real(rk), parameter :: XBIG   =  26.543E0_r8
   real(rk), parameter :: XHUGE  =   6.71E7_r8
   real(rk), parameter :: XMAX   =   2.53E307_r8

   
   
   
   real(rk), parameter :: A(5) = (/ 3.16112374387056560E00_rk, 1.13864154151050156E02_rk, &
                                    3.77485237685302021E02_rk, 3.20937758913846947E03_rk, &
                                    1.85777706184603153E-1_rk /)
   real(rk), parameter :: B(4) = (/ 2.36012909523441209E01_rk, 2.44024637934444173E02_rk, &
                                    1.28261652607737228E03_rk, 2.84423683343917062E03_rk /)

   
   
   
   real(rk), parameter :: C(9) = (/ 5.64188496988670089E-1_rk, 8.88314979438837594E00_rk, &
                                    6.61191906371416295E01_rk, 2.98635138197400131E02_rk, &
                                    8.81952221241769090E02_rk, 1.71204761263407058E03_rk, &
                                    2.05107837782607147E03_rk, 1.23033935479799725E03_rk, &
                                    2.15311535474403846E-8_rk /)
   real(rk), parameter :: D(8) = (/ 1.57449261107098347E01_rk, 1.17693950891312499E02_rk, &
                                    5.37181101862009858E02_rk, 1.62138957456669019E03_rk, &
                                    3.29079923573345963E03_rk, 4.36261909014324716E03_rk, &
                                    3.43936767414372164E03_rk, 1.23033935480374942E03_rk /)

   
   
   
   real(rk), parameter :: P(6) = (/ 3.05326634961232344E-1_rk, 3.60344899949804439E-1_rk, &
                                    1.25781726111229246E-1_rk, 1.60837851487422766E-2_rk, &
                                    6.58749161529837803E-4_rk, 1.63153871373020978E-2_rk /)
   real(rk), parameter :: Q(5) = (/ 2.56852019228982242E00_rk, 1.87295284992346047E00_rk, &
                                    5.27905102951428412E-1_rk, 6.05183413124413191E-2_rk, &
                                    2.33520497626869185E-3_rk /)

   
   X = ARG
   Y = ABS(X)
   IF (Y .LE. THRESH) THEN
      
      
      
      YSQ = ZERO
      IF (Y .GT. XSMALL) YSQ = Y * Y
      XNUM = A(5)*YSQ
      XDEN = YSQ
      DO I = 1, 3
         XNUM = (XNUM + A(I)) * YSQ
         XDEN = (XDEN + B(I)) * YSQ
      end do
      RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
      IF (JINT .NE. 0) RESULT = ONE - RESULT
      IF (JINT .EQ. 2) RESULT = EXP(YSQ) * RESULT
      GO TO 80
   ELSE IF (Y .LE. FOUR) THEN
      
      
      
      XNUM = C(9)*Y
      XDEN = Y
      DO I = 1, 7
         XNUM = (XNUM + C(I)) * Y
         XDEN = (XDEN + D(I)) * Y
      end do
      RESULT = (XNUM + C(8)) / (XDEN + D(8))
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   ELSE
      
      
      
      RESULT = ZERO
      IF (Y .GE. XBIG) THEN
         IF ((JINT .NE. 2) .OR. (Y .GE. XMAX)) GO TO 30
         IF (Y .GE. XHUGE) THEN
            RESULT = SQRPI / Y
            GO TO 30
         END IF
      END IF
      YSQ = ONE / (Y * Y)
      XNUM = P(6)*YSQ
      XDEN = YSQ
      DO I = 1, 4
         XNUM = (XNUM + P(I)) * YSQ
         XDEN = (XDEN + Q(I)) * YSQ
      end do
      RESULT = YSQ *(XNUM + P(5)) / (XDEN + Q(5))
      RESULT = (SQRPI -  RESULT) / Y
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   END IF
30 continue
   
   
   
   IF (JINT .EQ. 0) THEN
      RESULT = (HALF - RESULT) + HALF
      IF (X .LT. ZERO) RESULT = -RESULT
   ELSE IF (JINT .EQ. 1) THEN
      IF (X .LT. ZERO) RESULT = TWO - RESULT
   ELSE
      IF (X .LT. ZERO) THEN
         IF (X .LT. XNEG) THEN
            RESULT = XINF
         ELSE
            YSQ = AINT(X*SIXTEN)/SIXTEN
            DEL = (X-YSQ)*(X+YSQ)
            Y = EXP(YSQ*YSQ) * EXP(DEL)
            RESULT = (Y+Y) - RESULT
         END IF
      END IF
   END IF
80 continue
end SUBROUTINE CALERF_r8



SUBROUTINE CALERF_r4(ARG, RESULT, JINT)

   
   
   
   integer, parameter :: rk = r4

   
   real(rk), intent(in)  :: arg
   integer,  intent(in)  :: jint
   real(rk), intent(out) :: result

   
   INTEGER :: I

   real(rk) :: X, Y, YSQ, XNUM, XDEN, DEL

   
   
   
   real(rk), parameter :: ZERO   = 0.0E0_rk
   real(rk), parameter :: FOUR   = 4.0E0_rk
   real(rk), parameter :: ONE    = 1.0E0_rk
   real(rk), parameter :: HALF   = 0.5E0_rk
   real(rk), parameter :: TWO    = 2.0E0_rk
   real(rk), parameter :: SQRPI  = 5.6418958354775628695E-1_rk
   real(rk), parameter :: THRESH = 0.46875E0_rk
   real(rk), parameter :: SIXTEN = 16.0E0_rk

   
   
   
   real(rk), parameter :: XINF   =  3.40E+38_r4
   real(rk), parameter :: XNEG   = -9.382E0_r4
   real(rk), parameter :: XSMALL =  5.96E-8_r4 
   real(rk), parameter :: XBIG   =  9.194E0_r4
   real(rk), parameter :: XHUGE  =  2.90E3_r4
   real(rk), parameter :: XMAX   =  4.79E37_r4

   
   
   
   real(rk), parameter :: A(5) = (/ 3.16112374387056560E00_rk, 1.13864154151050156E02_rk, &
                                    3.77485237685302021E02_rk, 3.20937758913846947E03_rk, &
                                    1.85777706184603153E-1_rk /)
   real(rk), parameter :: B(4) = (/ 2.36012909523441209E01_rk, 2.44024637934444173E02_rk, &
                                    1.28261652607737228E03_rk, 2.84423683343917062E03_rk /)

   
   
   
   real(rk), parameter :: C(9) = (/ 5.64188496988670089E-1_rk, 8.88314979438837594E00_rk, &
                                    6.61191906371416295E01_rk, 2.98635138197400131E02_rk, &
                                    8.81952221241769090E02_rk, 1.71204761263407058E03_rk, &
                                    2.05107837782607147E03_rk, 1.23033935479799725E03_rk, &
                                    2.15311535474403846E-8_rk /)
   real(rk), parameter :: D(8) = (/ 1.57449261107098347E01_rk, 1.17693950891312499E02_rk, &
                                    5.37181101862009858E02_rk, 1.62138957456669019E03_rk, &
                                    3.29079923573345963E03_rk, 4.36261909014324716E03_rk, &
                                    3.43936767414372164E03_rk, 1.23033935480374942E03_rk /)

   
   
   
   real(rk), parameter :: P(6) = (/ 3.05326634961232344E-1_rk, 3.60344899949804439E-1_rk, &
                                    1.25781726111229246E-1_rk, 1.60837851487422766E-2_rk, &
                                    6.58749161529837803E-4_rk, 1.63153871373020978E-2_rk /)
   real(rk), parameter :: Q(5) = (/ 2.56852019228982242E00_rk, 1.87295284992346047E00_rk, &
                                    5.27905102951428412E-1_rk, 6.05183413124413191E-2_rk, &
                                    2.33520497626869185E-3_rk /)

   
   X = ARG
   Y = ABS(X)
   IF (Y .LE. THRESH) THEN
      
      
      
      YSQ = ZERO
      IF (Y .GT. XSMALL) YSQ = Y * Y
      XNUM = A(5)*YSQ
      XDEN = YSQ
      DO I = 1, 3
         XNUM = (XNUM + A(I)) * YSQ
         XDEN = (XDEN + B(I)) * YSQ
      end do
      RESULT = X * (XNUM + A(4)) / (XDEN + B(4))
      IF (JINT .NE. 0) RESULT = ONE - RESULT
      IF (JINT .EQ. 2) RESULT = EXP(YSQ) * RESULT
      GO TO 80
   ELSE IF (Y .LE. FOUR) THEN
      
      
      
      XNUM = C(9)*Y
      XDEN = Y
      DO I = 1, 7
         XNUM = (XNUM + C(I)) * Y
         XDEN = (XDEN + D(I)) * Y
      end do
      RESULT = (XNUM + C(8)) / (XDEN + D(8))
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   ELSE
      
      
      
      RESULT = ZERO
      IF (Y .GE. XBIG) THEN
         IF ((JINT .NE. 2) .OR. (Y .GE. XMAX)) GO TO 30
         IF (Y .GE. XHUGE) THEN
            RESULT = SQRPI / Y
            GO TO 30
         END IF
      END IF
      YSQ = ONE / (Y * Y)
      XNUM = P(6)*YSQ
      XDEN = YSQ
      DO I = 1, 4
         XNUM = (XNUM + P(I)) * YSQ
         XDEN = (XDEN + Q(I)) * YSQ
      end do
      RESULT = YSQ *(XNUM + P(5)) / (XDEN + Q(5))
      RESULT = (SQRPI -  RESULT) / Y
      IF (JINT .NE. 2) THEN
         YSQ = AINT(Y*SIXTEN)/SIXTEN
         DEL = (Y-YSQ)*(Y+YSQ)
         RESULT = EXP(-YSQ*YSQ) * EXP(-DEL) * RESULT
      END IF
   END IF
30 continue
   
   
   
   IF (JINT .EQ. 0) THEN
      RESULT = (HALF - RESULT) + HALF
      IF (X .LT. ZERO) RESULT = -RESULT
   ELSE IF (JINT .EQ. 1) THEN
      IF (X .LT. ZERO) RESULT = TWO - RESULT
   ELSE
      IF (X .LT. ZERO) THEN
         IF (X .LT. XNEG) THEN
            RESULT = XINF
         ELSE
            YSQ = AINT(X*SIXTEN)/SIXTEN
            DEL = (X-YSQ)*(X+YSQ)
            Y = EXP(YSQ*YSQ) * EXP(DEL)
            RESULT = (Y+Y) - RESULT
         END IF
      END IF
   END IF
80 continue
end SUBROUTINE CALERF_r4



FUNCTION DERF(X)








   integer, parameter :: rk = r8 

   
   real(rk), intent(in) :: X

   
   real(rk) :: DERF

   
   INTEGER :: JINT = 0
   

   CALL CALERF_r8(X, DERF, JINT)
END FUNCTION DERF



FUNCTION ERF_r4(X)








   integer, parameter :: rk = r4 

   
   real(rk), intent(in) :: X

   
   real(rk) :: ERF_r4

   
   INTEGER :: JINT = 0
   

   CALL CALERF_r4(X, ERF_r4, JINT)
END FUNCTION ERF_r4



FUNCTION DERFC(X)








   integer, parameter :: rk = r8 

   
   real(rk), intent(in) :: X

   
   real(rk) :: DERFC

   
   INTEGER :: JINT = 1
   

   CALL CALERF_r8(X, DERFC, JINT)
END FUNCTION DERFC



FUNCTION ERFC_r4(X)








   integer, parameter :: rk = r4 

   
   real(rk), intent(in) :: X

   
   real(rk) :: ERFC_r4

   
   INTEGER :: JINT = 1
   

   CALL CALERF_r4(X, ERFC_r4, JINT)
END FUNCTION ERFC_r4



FUNCTION DERFCX(X)








   integer, parameter :: rk = r8 

   
   real(rk), intent(in) :: X

   
   real(rk) :: DERFCX

   
   INTEGER :: JINT = 2
   

   CALL CALERF_r8(X, DERFCX, JINT)
END FUNCTION DERFCX



FUNCTION ERFCX_R4(X)








   integer, parameter :: rk = r4 

   
   real(rk), intent(in) :: X

   
   real(rk) :: ERFCX_R4

   
   INTEGER :: JINT = 2
   

   CALL CALERF_r4(X, ERFCX_R4, JINT)
END FUNCTION ERFCX_R4



end module error_function
