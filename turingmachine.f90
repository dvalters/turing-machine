! Turing machine implementation in Fortran
! Also used for example of OOP-features in Fortran 2003+

module class_MachineTape

  implicit none

  type, public :: MachineTape
    integer, dimension (:) :: tape
    integer :: tape_position
    character(len=1) :: blank_char
    character(len=:) :: initial_string
  ! define out type-bound procedures
    contains
      procedure :: init => tape_init
      procedure :: re_init => tape_reinit
      procedure :: move => tape_move
      procedure :: readin => tape_readin
      procedure :: move_left => tape_moveleft
      procedure :: move_right => tape_moveright
      procedure :: show => tape_show
  end type MachineTape

  ! implementation of the procedures
  contains
     subroutine tape_init(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_init

     subroutine tape_reinit(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_init

     subroutine tape_move(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_move

     subroutine tape_readin(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_readin

     subroutine tape_moveleft(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_moveleft

     subroutine tape_moveright(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_moveright

     subroutine tape_show(this)
       class(MachineTape), intent(in) :: this
     end subroutine tape_show

end module class_MachineTape


module class_TuringMachine

  implicit none

  !get the types and procedures
  use class_MachineTape

  type, public :: TuringMachine
    character(len=1) :: blank_char
    !Using the MachineTape class above
    type(MachineTape) :: mt
    !what about intialiser arguments?

    contains
      procedure :: init => machine_init
      procedure :: reinit => machine_reinit
      procedure :: add_transition => machine_addtransition
      procedure :: step => machine_step
      procedure :: execute => machine_execute
  end type TuringMachine

  subroutine machine_init(this)
    class(TuringMachine), intent(in) :: this
  end subroutine machine_init

  subroutine machine_reinit(this)
    class(TuringMachine), intent(in) :: this
  end subroutine machine_reinit

  subroutine machine_addtransition(this)
    class(TuringMachine), intent(in) :: this
  end subroutine machine_addtransition

  subroutine machine_step(this)
    class(TuringMachine), intent(in) :: this
  end subroutine machine_step

  subroutine machine_execute(this)
    class(TuringMachine), intent(in) :: this
  end subroutine machine_execute

end module class_TuringMachine


program main

  use class_TuringMachine
  implicit none

  type(TuringMachine) :: tm
  
  !now initialise the turing machine...
  tm = TuringMachine()

  call tm%add_transition()

  !etc.....
  call tm%execute

end program main
