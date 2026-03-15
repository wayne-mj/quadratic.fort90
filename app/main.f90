program main
  !> Quadratic equation solver (2024)
  use iso_fortran_env, only: real64
  use quadratic
  implicit none
 
  !> Define main variables as real 64bit
  real(kind=real64)               :: a,b,c
  type(quadtype)                  :: qt

  !> Character codes for formatting
  character (len=1), parameter    :: ESC = char(27)
  character (len=3), parameter    :: RESET = '[0m'
  character (len=3), parameter    :: ITALIC = '[3m'
  !> Format for irrational and ration results
  character (len=20), parameter   :: FMTIRRAT = "(A,F35.20,A,A,A,A,A)"
  character (len=10), parameter   :: FMTRAT = "(A,F35.20)"
   
  !> ax^2 + bx + c -> (-b +/- sqrt(b^2 - 4ac)) / 2a
  a = 2.
  b = -1.
  c = -5.

  qt = quad(a,b,c)
  
  
  !> Display the results for irrational results
  if (qt%irrat) then
    write (*, '(A)') "No real roots ..."
    write (*, FMTIRRAT) "X1: ",qt%x1,ESC,ITALIC,"i",ESC,RESET
    write (*, FMTIRRAT) "X2: ",qt%x2,ESC,ITALIC,"i",ESC,RESET
  !> Display the results for real results
  else 
    write (*, '(A)') "Roots are:"
    write (*, FMTRAT) "X1: ", qt%x1
    write (*, FMTRAT) "X2: ", qt%x2
  end if  
end program main

!> v0.0.1 Support for real solutions only
!> v0.0.3 Support for imaginary solutions added
!> v0.0.4 Corrected math to properly return discriminate
!> v0.0.7 Added formatting to output
!> v0.0.8 Added ANSI codes for formatting to distinguish special cases