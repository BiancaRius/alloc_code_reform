module types
   implicit none
   ! FOR THE GNU FORTRAN COMPILER
   integer,parameter,public :: l_1 = 2  ! standard Logical type
   integer,parameter,public :: i_2 = 2  ! 16 bits integer
   integer,parameter,public :: i_4 = 4  ! 32 bits integer
   integer,parameter,public :: r_4 = 4  ! 32 bits float
   integer,parameter,public :: r_8 = 8  ! 64 bits float

end module types

module params
    use types
    implicit none

    !allometric parameters
    real(r_8),parameter,public :: grid_area = 1000.0D0 !m2
    real(r_8),parameter,public :: pi   =  3.14159265D0
    real(r_8),parameter,public :: xacc =  0.1D0     !x-axis precision threshold for the allocation solution
    real(r_8),parameter,public :: yacc =  1.0D-10  !y-axis precision threshold for the allocation solution

    integer(i_4),parameter,public :: nseg = 20 ! segment number to bisection loop
    integer(i_4),parameter,public :: time = 1000
    integer(i_4),parameter,public :: ntl=365

    real(r_8),parameter,public :: allom1 = 100.D0
    real(r_8),parameter,public :: allom2 = 40.0D0
    real(r_8),parameter,public :: allom3 = 0.5D0
    real(r_8),parameter,public :: latosa = 8000.0D0
    real(r_8),parameter,public :: reinickerp = 1.6D0
    real(r_8),parameter,public :: ltor = 0.77302587552347657D0 !leaf:root from Philip

    !allometric parameters for saplins (establishment module)
    real(r_8),parameter,public :: klatosa_sapl = 8000.0D0

    real(r_8),parameter,public :: k_allom1_sapl = 100.0D0

    real(r_8),parameter,public :: k_allom2_sapl = 40.0D0

    real(r_8),parameter,public :: k_allom3_sapl = 0.50D0

    real(r_8),parameter,public :: x_sapl = 3. !from lpjlmfire (pftparametersmod.f90)

    real(r_8),parameter,public :: reinickerp_sapl = 1.60D0

    real(r_8),parameter,public :: lai_sapl = 4.0D0 !lpjmlfire (pft parameter)

    real(r_8),parameter,public :: sla_sapl = 0.021 !from lpjlmfire (pftparametersmod.f90, line 229) m2/gC

    real(r_8),parameter,public :: dwood_sapl = 2e5 !gc/m3

    real(r_8),parameter,public :: lmtorm_sapl = 1.0D0


    !establishment parameters
    real(r_8),parameter,public :: est_max = 0.24D0 !maximum establishment/year !based on Sitch et al 2003
                                                   !OR (need to decide which one will be used)
                                                   !2 individuals m -2 yr -1 - reference: Levis et al 2004 (Eq 53)
    real(r_8),parameter,public :: p1 = -5.0D0 !parameter to establishment equation !based on Sitch et al 2003
    real(r_8),parameter,public :: gc_area = 10000.0D0 !meters !grid cell area in which the ecophysiological processes
                                                      !occur. 1ha: 10.000m2
    real(r_8),parameter,public :: gc_area_95 = gc_area*0.95 !95% of gc_area. The limit of gc_area that all PLS
                                                        !are allowed to occupy

    !FPC parameters
    !initial parameters
    real(r_8),parameter,public :: leaf_alloc_init = 0.40D0
    real(r_8),parameter,public :: root_alloc_init = 0.30D0
    real(r_8),parameter,public :: wood_alloc_init = 0.30D0

    !mortality parameters
    real(r_8),parameter,public :: k_mort2 = 0.50D0
end module params