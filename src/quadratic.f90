module quadratic
  use iso_fortran_env, only: real64
  implicit none
  !> Set default state to private
  private

  !> Expose the functions, types, and anything else
  public :: quad, quadtype
  
  !> Datatype for quadratic
  type :: quadtype
    real(kind=real64)   :: x1, x2
    logical             :: irrat
  end type quadtype

contains
  !> Function quadratic
  function quad (a,b,c) result(xs)
    real (kind=real64), intent(in)  :: a, b, c      !> a, b, c real 64 bit 
    complex                         :: z1, z2       !> z1, z2 complex for negative discriminate
    real (kind=real64)              :: d            !> Discriminate
    type(quadtype)                  :: xs           !> Return data type

    !> Initialise the variables to 0
    d = 0.
    z1 = (0.,0.)
    z2 = (0.,0.)

    !> Ensure that a is not equal to 0
    if (a .eq. 0.) then
      return
    else
      !> Calculate the discriminate
      d = (b**2) - (4 * a * c)
      !> If negative, then no real root
      if (d .lt. 0.) then
        z1 = complex(d,0)
        z2 = sqrt(z1)
        xs%x1 = (-(b) - aimag(z2)) / 2 * a
        xs%x2 = (-(b) + aimag(z2)) / 2 * a
        xs%irrat = .true.
      !> Else >=0 so 1 or more roots
      else 
        xs%x1 = (-(b) - sqrt(d)) / 2 * a
        xs%x2 = (-(b) + sqrt(d)) / 2 * a
        xs%irrat = .false.
      end if
    end if
  end function
end module quadratic
