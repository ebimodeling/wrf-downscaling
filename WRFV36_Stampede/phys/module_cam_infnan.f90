



module infnan















  use shr_kind_mod, only: r8 => shr_kind_r8






  real(r8), parameter :: inf = 1.e19
  real(r8), parameter :: nan = 1.e21
  integer,  parameter :: bigint = O'17777777777'






  real(r8), parameter :: uninit_r8 = inf                   
end module infnan
