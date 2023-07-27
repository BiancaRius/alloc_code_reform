program program2
    use constants
    use types
    use allocation
    use gridcell_occupation
    use allometry_structure
    
    implicit none
    integer(i_4) :: i

    !C that enters in the logic (kgC/m2) for each PLS (attention it is not individual yet)
    real(r_8) :: leaf_in  = 0.01 !kgC/m2 initial C leaf input 
    real(r_8) :: root_in  = 0.01 !kgC/m2 initial C root input
    real(r_8) :: sap_in = 15.
    real(r_8) :: heart_in = 40.
    real(r_8) :: storage_in = 0.5
    real(r_8) :: bminc_in = 0.8/365.242D0!0.0004 !carbon (NPP) available to be allocated
                                !basically NPPt - NPPt-1. NPP accumulated in the year/month/day
                                !gc/ind/time_step

    !density that enters in the logic (ind/m2) for each PLS
    real(r_8) :: dens_in = 1. !ind/m2 initial density of individuals 

    !carbon (gC) in compartments considering the density (ind/m2)
    real(r_8) :: leaf_in_ind
    real(r_8) :: root_in_ind
    real(r_8) :: sap_in_ind
    real(r_8) :: heart_in_ind
    real(r_8) :: storage_in_ind
    real(r_8) :: wood_in_ind !(gC) total wood - sum of heart and sap

    !C that is an output from the logic (kgC/m2) for each PLS
    real(r_8) :: leaf_out
    real(r_8) :: root_out
    real(r_8) :: sap_out
    real(r_8) :: heart_out
    real(r_8) :: storage_out

    !density that is an output from the logic (ind/m2) for each PLS
    real(r_8) :: dens_out

    
    !initializing variables
    leaf_in_ind = 0.0D0
    root_in_ind = 0.0D0
    sap_in_ind = 0.0D0
    heart_in_ind = 0.0D0
    wood_in_ind = 0.0D0
    storage_in_ind = 0.0D0
! 
    ! carbon (gC) in compartments considering the density (ind/m2)
    leaf_in_ind = (leaf_in/dens_in)*1.D3
    root_in_ind = (root_in/dens_in)*1.D3
    sap_in_ind = (sap_in/dens_in)*1.D3 
    heart_in_ind = (heart_in/dens_in)*1.D3
    storage_in_ind = (storage_in/dens_in)*1.D3
    wood_in_ind = sap_in_ind + heart_in_ind

    ! bminc_in_ind = (bminc_in/dens_in)*1D3


    do i = 1, 30
      print*, ' '
      print*,'STEP', i
      call alloc(leaf_in, root_in, sap_in, heart_in, storage_in, bminc_in,dens_in,&
          leaf_out, root_out, sap_out, heart_out, storage_out)

      !update variables
        leaf_in = leaf_out
        root_in = root_out
        sap_in = sap_out
        storage_in = storage_out
        heart_in = heart_out

      !enters to calculate gc_occupation
      
      
        ! print*, 'bminc ======', bminc_in/dens_in, i
        PRINT*, '____________________________________'
        print*,'leaf updt===========', leaf_in,  i
        print*,'root updt===========', root_in,  i
        print*,'sap  updt===========', sap_in,  i
        print*,'storage  updt===========', storage_in,  i
! 
        PRINT*, '____________________________________'

    enddo

    call gc_occupation(leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind, wood_in_ind)


    !!!Sequence of calls

    !call allocation
    
    !call gc_occupation
    !call FPC
      !if gt than area
        !call exc_area
      !else
        !call establishment
        !call shrink

    !call mortality - percentage/ calculates mortality due to gc occupation, greff, and wood density

    !update carbon pools due to mortality (percentage)

    !lose tissue through turnover
        
       

end program program2
