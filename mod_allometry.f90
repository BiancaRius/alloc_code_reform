!Module to calculated the area occupied by the PLSs

module allometry_structure

    use constants
    use types
    use traits !provisory value for traits

implicit none

private

public :: allometry

contains

    subroutine allometry()
        
        ! !kgC/m2 - PLS variable
        ! real(r_8), intent(in) :: leaf_in
        ! real(r_8), intent(in) :: root_in
        ! real(r_8), intent(in) :: sap_in
        ! real(r_8), intent(in) :: heart_in

        ! !ind/m2 density of individuals
        ! real(r_8), intent(in) :: dens_in
       
        ! diam(j,k) = (4*(cs2(j,k)+ch2(j,k)) / (dwood(j,k)*1000000.) / pi / k_allom2)**(1./(2. + k_allom3)) !Eqn 9
        ! !print*, 'diam lpjmfire', diam(j,k)*100
        

        ! crown_area(j,k) = min(crown_area_max,k_allom1 * diam(j,k)**krp)
        ! !crown_area(j,k)=k_allom1 * diam(j,k)**krp
        ! !print*,'ca lpj', crown_area(j,k)

        ! lai(j,k) = (cl2(j,k)*spec_leaf(j,k))/crown_area(j,k)
        ! !print*, 'lai', lai(j,k)
        
        ! height(j,k) = k_allom2 *diam(j,k)**k_allom3
       
        ! diam(j,k) = ((4*(cw2(j,k)))/((dwood(j,k)*1000000.)*3.14*36))**(1/(2+0.22)) !nessa equação dwood deve estar em *g/m3*
            ! height
                ! crown_area(j,k) = k_allom1*(diam(j,k)**krp)
            
                ! lai(j,k) = (cl2(j,k)*spec_leaf(j,k))/crown_area(j,k)         

    end subroutine allometry

end module allometry_structure

