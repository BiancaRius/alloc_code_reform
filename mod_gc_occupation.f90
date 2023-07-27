!Module to calculated the area occupied by the PLSs

module gridcell_occupation

    use constants
    use types
    use traits !provisory value for traits
    use allometry_structure

implicit none

private

public :: gc_occupation

contains

    subroutine gc_occupation(leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind, wood_in_ind)
        
        !carbon (gC) in compartments considering the density (ind/m2)
        real(r_8), intent(in) :: leaf_in_ind
        real(r_8), intent(in) :: root_in_ind
        real(r_8), intent(in) :: sap_in_ind
        real(r_8), intent(in) :: heart_in_ind
        real(r_8), intent(in) :: wood_in_ind


        call allometry()


        

    end subroutine gc_occupation

end module gridcell_occupation

