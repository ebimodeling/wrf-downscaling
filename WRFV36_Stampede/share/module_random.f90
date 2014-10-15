module module_random
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  
  
  
  

  
  
  

  

  

  interface rand_grid
     module procedure rand_grid_i4
     module procedure rand_grid_r4
     module procedure rand_grid_i8
     module procedure rand_grid_r8
  end interface

contains
  subroutine srand_grid(state1,state2,state3,state4, &
                           IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE,seed)

    
    
    
    
    
    
    

    
    

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(in), optional :: seed
    integer(kind=4) :: iseed
    integer :: i,j,k
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE
    integer :: seeds(its:ite),n

    if(present(seed)) then
       iseed=seed
    else
       iseed=0
    endif

    n=ite-its+1

    
    do k=kts,kte
       do j=jts,jte
          do i=its,ite
             seeds(i)=(kde-kds+1)*((jde-jds+1)*i+j)+k
             
             
             
             
          enddo
          call bobraninit(state1(its,j,k),state2(its,j,k), &
                          state3(its,j,k),state4(its,j,k), &
                          seeds,seed,n)
       enddo
    enddo
  end subroutine srand_grid

  subroutine rand_grid_r4(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    
    
    
    
    

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    real(kind=4),    intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_r4(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_r4

  subroutine rand_grid_i4(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    
    
    
    
    
    
    
    
    
    
    
    
    
    

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_i4(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_i4

  subroutine rand_grid_r8(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    
    
    
    
    

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    real(kind=8),    intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_r8(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_r8

  subroutine rand_grid_i8(state1,state2,state3,state4,randdat, &
                          IDS,IDE,JDS,JDE,KDS,KDE, &
                          IMS,IME,JMS,JME,KMS,KME, &
                          ITS,ITE,JTS,JTE,KTS,KTE)

    
    
    
    
    
    
    
    
    
    
    
    
    
    

    implicit none
    integer(kind=4), intent(inout) :: state1(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state2(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state3(ims:ime,jms:jme,kms:kme)
    integer(kind=4), intent(inout) :: state4(ims:ime,jms:jme,kms:kme)
    integer(kind=8), intent(inout) :: randdat(ims:ime,jms:jme,kms:kme)
    integer :: i,j,k,n
    INTEGER, intent(in) :: IDS,IDE,JDS,JDE,KDS,KDE, &
                           IMS,IME,JMS,JME,KMS,KME, &
                           ITS,ITE,JTS,JTE,KTS,KTE

    n=ite-its+1

    do k=kts,kte
       do j=jts,jte
          call bobranval_i8(state1(its,j,k),state2(its,j,k), &
                            state3(its,j,k),state4(its,j,k), &
                            randdat(its,j,k),n)
       enddo
    enddo

  end subroutine rand_grid_i8

end module module_random
