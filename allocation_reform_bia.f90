module allocation

    use constants
    use types
    use traits

implicit none

private

public :: alloc,& !(s) calculates carbon pools output from NPP and carbon from preivous step
          height_calc,& !(f)calculates height
          leaf_req_calc,& !(f)leaf mass requeriment to satisfy allometry
          leaf_inc_min_calc,& !(f) minimum leaf increment to satisfy allocation equations
          root_inc_min_calc, & !(f) minimum root increment to satisfy allocation equations
          normal_alloc, & !(f)regular allocation process
          abnormal_alloc,& !abnormal allocation
          root_bisec_calc,& !solves bisection method for normal allocation
          positive_leaf_inc_min,&
          mortality_turnover,& !(s) accounts for mortality through turnover
          storage_accumulation,& !allocation for storage compartment when normal allocation is not possible
          reallocation !use of storage when normal allocation is not possible  

contains

    !if NPP .gt. 0 
        !if leaf & root requirement .gt. 0
            !if NPP .gt. leaf + root requirement
                !if minimum nutrients for allocation
                    !call allocation            
                !else
                    !!call storage_accumulation (non allocated NPP goes to storage)
                    
            !else
                !call reallocation
        !else
            !call storage_accumulation (non allocated NPP goes to storage)
    !else
        !call reallocation 

    !endif


    !!!!!inside reallocation!!!!

    !if Storage + NPP .lt. leaf + root requirement .and. nutrients .lt. minimumrequirement
        !call storage_accumulation (non allocated NPP goes to storage)
    !else
        !continue to the program

    !endif


    subroutine alloc(leaf_in, root_in, sap_in, heart_in, storage_in, bminc_in, dens_in,&
        leaf_out, root_out, sap_out, heart_out, storage_out)

        !integer(i_4), intent(in)::z 
        !VARIABLE INPUTS
        !carbon inputs (kgC/m2)
        real(r_8), intent(in) :: leaf_in
        real(r_8), intent(in) :: root_in
        real(r_8), intent(in) :: sap_in
        real(r_8), intent(in) :: heart_in
        real(r_8), intent(in) :: storage_in

        real(r_8), intent(in) :: dens_in !ind/m2 initial density of individuals

        real(r_8), intent(in) :: bminc_in !carbon (NPP) available to be allocated
                                          !basically NPPt - NPPt-1. NPP accumulated in the year/month/day
                                          !gc/ind/time_step
       
        !VARIABLES OUTPUTS 
        !carbon inputs (kgC/m2)
        real(r_8), intent(out) :: leaf_out
        real(r_8), intent(out) :: root_out
        real(r_8), intent(out) :: sap_out
        real(r_8), intent(out) :: heart_out
        real(r_8), intent(out) :: storage_out

        !INTERNAL VARIABLES
        real(r_8) :: wood_in_ind !(gC) total wood - sum of heart and sap

        !variable for bisec loop
        integer(i_4) :: x
        real(r_8) :: tmp
        real(r_8) :: tmp2
        real(r_8) :: sens

        !carbon (gC) in compartments considering the density (ind/m2)
        real(r_8) :: leaf_in_ind
        real(r_8) :: root_in_ind
        real(r_8) :: sap_in_ind
        real(r_8) :: heart_in_ind
        real(r_8) :: storage_in_ind
        real(r_8) :: realloc !provisory variable 


        real(r_8) :: bminc_in_ind

        !Functions to the logic
        !dwood !!####****!! ATTENTION: dwood is already transformed to gC/m3 at constants.f90
        real(r_8) :: height
        real(r_8) :: leaf_req
        real(r_8) :: leaf_inc_min
        real(r_8) :: root_inc_min

        !variables allocation (increase for each compartment)
        real(r_8) :: leaf_inc_alloc
        real(r_8) :: root_inc_alloc
        real(r_8) :: sap_inc_alloc
        real(r_8) :: heart_inc_alloc
        real(r_8) :: storage_inc_alloc

        !C available (NPP + storage)
        real(r_8) :: c_available

        !variable update that goes to turnover mortality (compartment_in_ind + compartiment_inc_alloc)
        real(r_8) :: leaf_updt
        real(r_8) :: root_updt
        real(r_8) :: sap_updt
        real(r_8) :: heart_updt
        real(r_8) :: storage_updt

        !amount of C lost with turnover processes
        real(r_8) :: leaf_turn
        real(r_8) :: root_turn
        real(r_8) :: sap_turn
        real(r_8) :: heart_turn
        real(r_8) :: storage_turn



        !initializing variables
        leaf_in_ind = 0.0D0
        root_in_ind = 0.0D0
        sap_in_ind = 0.0D0
        heart_in_ind = 0.0D0
        wood_in_ind = 0.0D0
        storage_in_ind = 0.0D0

        leaf_inc_min = 0.0D0
        root_inc_min = 0.0D0

        leaf_out = 0.0D0
        root_out = 0.0D0
        sap_out  = 0.0D0
        heart_out = 0.0D0
        storage_out = 0.0D0
 
        height = 0.0D0
        leaf_req = 0.0D0

        leaf_inc_alloc = 0.0D0
        root_inc_alloc = 0.0D0
        sap_inc_alloc = 0.0D0
        heart_inc_alloc = 0.0D0
        storage_inc_alloc = 0.0D0

        c_available = 0.0D0

        leaf_updt = 0.0D0
        root_updt = 0.0D0
        sap_updt = 0.0D0
        heart_updt = 0.0D0
        storage_updt = 0.0D0

        leaf_turn = 0.0D0
        root_turn = 0.0D0
        sap_turn = 0.0D0
        heart_turn = 0.0D0
        storage_turn = 0.0D0

        !carbon (gC) in compartments considering the density (ind/m2)
        leaf_in_ind = (leaf_in/dens_in)*1.D3
        root_in_ind = (root_in/dens_in)*1.D3
        sap_in_ind = (sap_in/dens_in)*1.D3 
        heart_in_ind = (heart_in/dens_in)*1.D3
        storage_in_ind = (storage_in/dens_in)*1.D3
        wood_in_ind = sap_in_ind + heart_in_ind

        bminc_in_ind = (bminc_in/dens_in)*1D3

        ! call functions to logic
        height = height_calc(wood_in_ind)

        !leaf requirement
        leaf_req = leaf_req_calc(sap_in_ind, height)

        !minimum increment to leaf
        leaf_inc_min = leaf_inc_min_calc(leaf_req, leaf_in_ind)
        print*, 'leaf_inc_min', leaf_inc_min

        !minimum increment to root
        root_inc_min = root_inc_min_calc(leaf_req, root_in_ind)
        print*, 'root_inc_min', root_inc_min

        print*, 'sum leaf and root min', leaf_inc_min + root_inc_min


    !!conditions for allocation!!!


        if (leaf_inc_min.gt.0.0D0.and.root_inc_min.gt.0.0D0) then

            print*, 'leaf and root inc minimum are > 0'

            if((bminc_in_ind.gt.0)) then

                print*, 'NPP > 0.'
                
                if (bminc_in_ind.ge.(root_inc_min + leaf_inc_min)) then

                    print*, 'NPP > sum of root and leaf inc min'
                    
                    !if minimum nutrients

                    call normal_alloc(leaf_inc_min, leaf_in_ind, root_in_ind, bminc_in_ind,&
                    sap_in_ind, heart_in_ind, leaf_inc_alloc, root_inc_alloc, sap_inc_alloc)
                   
                    !else
                        
                        !storage = storage + bminc    
                    !endif

                else
                    ! call abnormal_alloc(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind,height,&
                    ! leaf_inc_alloc, root_inc_alloc, sap_inc_alloc,heart_inc_alloc)
                    
                    print*, 'NPP < sum of root and leaf inc min'

                    if ( (storage_in_ind + bminc_in_ind).ge.(root_inc_min + leaf_inc_min) ) then !!AND NUTRIENTS
                        !call reallocation(storage_in_ind, realloc)
                       !storage_inc_alloc = bminc - (inc_leaf + inc_root)

                        print*, 'reallocation'

                        
                        
                        call reallocation(storage_in_ind, bminc_in_ind, leaf_inc_min, root_inc_min,&
                        leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc, storage_inc_alloc)

                        ! call reallocation(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind, height, storage_in_ind,&
                        ! leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc, storage_inc_alloc)

                        print*, 'use storage and discount leaf inc and root inc'

                    else

                        print*, 'non used npp goes to storage'
                        storage_inc_alloc = bminc_in_ind 

                    end if
                   
                end if

            else
                ! call abnormal_alloc(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind,height,&
                ! leaf_inc_alloc, root_inc_alloc, sap_inc_alloc,heart_inc_alloc)
                
                print*, 'NPP < 0' !se a NPP for negativa, então o valor dela é descontado do storage

                if ( (storage_in_ind + bminc_in_ind).ge.(root_inc_min + leaf_inc_min) ) then !!AND NUTRIENTS
                        !call reallocation(storage_in_ind, realloc)

                        !storage_inc_alloc = bminc - (inc_leaf + inc_root)
                    print*, 'reallocation'

                    call reallocation(storage_in_ind, bminc_in_ind, leaf_inc_min, root_inc_min,&
                    leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc, storage_inc_alloc)

                    ! call reallocation(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind, height, storage_in_ind,&
                    ! leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc, storage_inc_alloc)

                    print*, 'use storage and discount leaf inc and root inc'
                
                else
                    
                    print*, 'see what happens in CAETE now'
                     
                end if
                    
            endif

        else 

            print*, 'leaf and root inc minimum are < 0'

            if ( bminc_in_ind.gt.0.0D0 ) then

                print*, 'NPP > 0. -> non allocated goes to storage'
                storage_inc_alloc = bminc_in_ind
                
            else 

                print*, 'NPP < 0 se what happens in the model'

            end if    
            
           

        endif
    
    !!!end of conditions for allocation!!!!

       
        ! if (root_inc_min .gt. 0.0D0 .and. leaf_inc_min .gt. 0.0D0 .and. &
        !         & (root_inc_min + leaf_inc_min) .lt. bminc_in_ind) then
        !     ! print*, 'normal'
            
        !     call normal_alloc(leaf_inc_min, leaf_in_ind, root_in_ind, bminc_in_ind,&
        !             sap_in_ind, heart_in_ind, leaf_inc_alloc, root_inc_alloc, sap_inc_alloc)

        !             ! print*, 'leaf inc alloc==', leaf_inc_alloc
        ! else

        !     ! print*, ' abnormal'
        !     call abnormal_alloc(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind,height,&
        !     leaf_inc_alloc, root_inc_alloc, sap_inc_alloc,heart_inc_alloc)
            
           

        ! endif

         !Update variable and add Increment to C compartments 
        leaf_updt  = leaf_in_ind + leaf_inc_alloc
        root_updt  = root_in_ind + root_inc_alloc
        sap_updt   = sap_in_ind + sap_inc_alloc
        heart_updt = heart_in_ind + heart_inc_alloc
        storage_updt = storage_in_ind + storage_inc_alloc
        print*, 'storage updt', storage_updt, storage_in_ind, bminc_in_ind

        !mortality through turnover
        call mortality_turnover(leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind,storage_in_ind,&
            leaf_turn, root_turn, sap_turn, heart_turn, storage_turn)

        !discout C due to turnover and transform variable in kgC/m2 to ouput
        leaf_out = ((leaf_updt - leaf_turn)*dens_in)/1.D3
        root_out = ((root_updt - root_turn)*dens_in)/1.D3
        sap_out  = ((sap_updt - sap_turn)*dens_in)/1.D3
        !!!ATENÇÃO: CORRIGIR HEART_INC_ALLOC PRA ALLOC NORMAL
        heart_out = (((heart_updt - heart_turn) + sap_turn)*dens_in)/1.D3
        storage_out = ((storage_updt - storage_turn) * dens_in)/1.D3

        ! print*, '__________________________'
        ! print*, 'leaf out', leaf_out
        ! print*, 'sap out', sap_out
        ! print*, 'root out', root_out
        ! print*, 'heart out', heart_out 
        ! print*, '__________________________'

        !________________
        !sensitivity test
        ! x = 0
        ! tmp  = 0.2
        ! tmp2 = 1
        ! sens = 1.e-3

        ! do i = 1, 200 
            
        ! !    if (x.eq.200) exit 
        
        !    if ((abs(tmp - tmp2)).le.sens) then
        !     print*, 'sensitivity attained'
        !     exit 
        !    endif 
           
        !    !IFS normal/abnormal allocation
        !    !sensitivity of carbon (yes)
        !    ! put scape infinity loop (print)
        !    tmp = tmp2 
           
        !    x = x + 1
           
        !    tmp2 = (tmp2 + 4)/2

        ! enddo

        ! print*, x
        ! !_________________

    end subroutine alloc

    function height_calc (wood_in_ind) result (height)
        
        real(r_8), intent(in) :: wood_in_ind !gC/ind - total wood (sap + heart) carbon stock
        
        !Trait
        !dwood - wood density
        
        real(r_8) :: height !m - output

        !variable internal
        real(r_8) :: diameter 

        !initializing variables
        diameter = 0.0D0
        height = 0.0D0
        
        !Calculo diameter (necessary to height)
        diameter = ((4*wood_in_ind)/(dwood)*pi*k_allom2)**(1/(2+k_allom3))

        !Height 
        height = k_allom2*(diameter**k_allom3)


    end function

    function leaf_req_calc (sap_in_ind, height)  result (leaf_req)
    
        real(r_8), intent(in) :: sap_in_ind !gC - sapwood input
        real(r_8), intent(in) :: height !m
       
        real(r_8) :: leaf_req !gC - output- leaf mass requeriment to satisfy allometry 

        !Traits from constants.f90
        !dwood - wood density (gc/m3) - already transformed in constants.f90
        !sla - specific leaf area

        !initializing variables
        leaf_req = 0.0D0

        leaf_req = klatosa * sap_in_ind / (dwood * height * sla)

    end function leaf_req_calc

    function leaf_inc_min_calc (leaf_req, leaf_in_ind) result (leaf_inc_min)   

        real(r_8), intent(in) :: leaf_req !gC leaf mass requeriment to satisfy allometry
        real(r_8), intent(in) :: leaf_in_ind !gC leaf input
        
        real(r_8) :: leaf_inc_min !gC -output- minimum leaf increment to satisfy allocation equations

        !initializing variables
        leaf_inc_min = 0.0D0

        leaf_inc_min = leaf_req - leaf_in_ind

    end function leaf_inc_min_calc

    function root_inc_min_calc (leaf_req, root_in_ind) result (root_inc_min) !ROOT MASS MINIMO
        
        !calculate minimum root production to support this leaf mass (i.e. lm_ind + lminc_ind_min)
        !May be negative following a reduction in soil water limitation (increase in lm2rm) relative to last year.

        real(r_8), intent(in) :: root_in_ind !gC root input
        real(r_8), intent(in) :: leaf_req  !gC leaf mass requeriment to satisfy allometry
        
        real(r_8) :: root_inc_min !gC -output- minimum root increment to satisfy allocation equations

        root_inc_min = leaf_req / ltor - root_in_ind
       

    end function root_inc_min_calc

    subroutine normal_alloc (leaf_inc_min, leaf_in_ind, root_in_ind, bminc_in_ind,&
        sap_in_ind, heart_in_ind, leaf_inc_alloc, root_inc_alloc, sap_inc_alloc)

        real(r_8), intent(in) :: leaf_inc_min 
        real(r_8), intent(in) :: leaf_in_ind  
        real(r_8), intent(in) :: root_in_ind
        real(r_8), intent(in) :: sap_in_ind  
        real(r_8), intent(in) :: heart_in_ind
        real(r_8), intent(in) :: bminc_in_ind

        real(r_8), intent(out) :: leaf_inc_alloc
        real(r_8), intent(out) :: root_inc_alloc
        real(r_8), intent(out) :: sap_inc_alloc


        real(r_8) :: x1
        real(r_8) :: x2
        real(r_8) :: dx
        real(r_8) :: fx1     


        !initializing variables
        x1 = 0.0D0
        x2 = 0.0D0
        dx = 0.0D0
        leaf_inc_alloc = 0.0D0
        root_inc_alloc = 0.0D0
        sap_inc_alloc = 0.0D0
        fx1 = 0.0D0


        x1 = leaf_inc_min

        x2 = (bminc_in_ind - (leaf_in_ind/ ltor - root_in_ind))/ (1. + 1. / ltor )

        dx = x2 - x1

        if (dx < 0.01) then !0.01 é a precisão da bisection. 

            !there seems to be rare cases where lminc_ind_min (x1) is almost equal to x2. In this case,
            !assume that the leafmass increment is equal to the midpoint between the values and skip 
            !the root finding procedure

            leaf_inc_alloc = x1 + 0.5 * dx


        else 
            !Find a root for non-negative lminc_ind, rminc_ind and sminc_ind using Bisection Method (Press et al., 1986, p 346)
            !There should be exactly one solution (no proof presented, but Steve has managed one).

            call positive_leaf_inc_min(leaf_in_ind, sap_in_ind, heart_in_ind,&
            root_in_ind, bminc_in_ind, dx, x1, x2, leaf_inc_alloc)        
            
            root_inc_alloc = ((leaf_in_ind + leaf_inc_alloc) / ltor) - root_in_ind

            sap_inc_alloc = bminc_in_ind - leaf_inc_alloc - root_inc_alloc

        endif


    end subroutine normal_alloc

    function root_bisec_calc (leaf_in_ind, sap_in_ind, heart_in_ind, root_in_ind,&
        bminc_in_ind, x) result (fx1)

        real(r_8), intent(in) :: leaf_in_ind 
        real(r_8), intent(in) :: sap_in_ind
        real(r_8), intent(in) :: heart_in_ind 
        real(r_8), intent(in) :: root_in_ind 
        real(r_8), intent(in) :: bminc_in_ind 
        real(r_8), intent(in) :: x
        
        real(r_8) :: fx1 !output
        
        !internal variables
        real(r_8), parameter :: pi4 = pi/4
        real(r_8), parameter :: a1 = 2./ k_allom3
        real(r_8), parameter :: a2 = 1. + a1 !Essa é a forma correta !!CONFERIR ...ESTÁ DIFERENTE ENTRE NOSSO CÓDIGO E O lpjmlFIRE
        real(r_8), parameter :: a3 = k_allom2**a1

        
        !initializing variables
        fx1 = 0.0D0

        fx1 = a3 * ((sap_in_ind + bminc_in_ind - x - ((leaf_in_ind + x)/ltor) + root_in_ind + heart_in_ind) / dwood)/ pi4 - &
                    ((sap_in_ind + bminc_in_ind - x - ((leaf_in_ind + x)/ ltor) + root_in_ind) / ((leaf_in_ind + x)&
                    * sla * dwood / klatosa)) ** a2
       

    end function root_bisec_calc

    subroutine positive_leaf_inc_min (leaf_in_ind, sap_in_ind, heart_in_ind,&
        root_in_ind, bminc_in_ind, dx2, x1_aux, x2_aux, leaf_inc_alloc)

        real(r_8), intent(in) :: leaf_in_ind 
        real(r_8), intent(in) :: sap_in_ind
        real(r_8), intent(in) :: heart_in_ind 
        real(r_8), intent(in) :: root_in_ind 
        real(r_8), intent(in) :: bminc_in_ind 
        real(r_8), intent(in) :: x1_aux, x2_aux 
        real(r_8), intent(in) :: dx2

        real(r_8), intent(out) :: leaf_inc_alloc

        !internal variable
        real(r_8) :: dx
        real(r_8) :: fx1
        real(r_8) :: fmid
        real(r_8) :: xmid
        real(r_8) :: x1
        real(r_8) :: x2
        real(r_8) :: sign
        real(r_8) :: rtbis


        integer(i_4) :: i

        x1 = x1_aux
        x2 = x2_aux

        ! print*,'bf', x1, x2

        dx = dx2 / real(nseg)
            
        fx1 = root_bisec_calc(leaf_in_ind, sap_in_ind, heart_in_ind,&
            root_in_ind, bminc_in_ind, x1)

        !Find approximate location of leftmost root on the interval (x1,x2).
        !Subdivide (x1,x2) into nseg equal segments seeking change in sign of f(xmid) relative to f(x1).

        fmid = fx1
        xmid = x1

        i = 1
            
        do 
            xmid = xmid + dx
            
            fmid = root_bisec_calc(leaf_in_ind, sap_in_ind, heart_in_ind,&
                root_in_ind, bminc_in_ind, xmid)

            i = i + 1
            
            if (fmid * fx1 .le. 0. .or. xmid .ge. x2) exit  !sign has changed or we are over the upper bound

            if (i > 20) print*, 'first alloc loop flag'
            if (i > 100) stop 'Too many iterations allocmod'
      

        end do
        print*, i
        !the interval that brackets zero in f(x) becomes the new bounds for the root search

        x1 = xmid - dx
        x2 = xmid

        !Apply bisection method to find root on the new interval (x1,x2)
        fx1 = root_bisec_calc(leaf_in_ind, sap_in_ind, heart_in_ind,&
        root_in_ind, bminc_in_ind, x1)

        if (fx1.ge.0.) then
            sign = -1
        else
            sign = 1
        endif

        rtbis = x1

        dx = x2 - x1

        !Bisection loop: search iterates on value of xmid until xmid lies within xacc of the root,
        !i.e. until |xmid-x| < xacc where f(x) = 0. the final value of xmid with be the leafmass increment

        i = 1

        do 
            dx   = 0.5 * dx
            xmid = rtbis + dx

            !calculate fmid = f(xmid) [eqn (22)]

            fmid = root_bisec_calc(leaf_in_ind, sap_in_ind, heart_in_ind,&
                root_in_ind, bminc_in_ind, xmid)

            if (fmid * sign .le. 0.) rtbis = xmid

            if (dx .lt. xacc .or. abs(fmid) .le. yacc) exit

            if (i > 20) print*,'second alloc loop flag'
            if (i > 50) stop 'Too many iterations allocmod'
  

            i = i + 1
        end do
        
        !Now rtbis contains numerical solution for lminc_ind given eqn (22)

        leaf_inc_alloc = rtbis

    
    end subroutine

    subroutine abnormal_alloc(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind,heart_in_ind, height,&
        leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc)
        
        
        real(r_8), intent(in) :: leaf_in_ind 
        real(r_8), intent(in) :: sap_in_ind
        real(r_8), intent(in) :: root_in_ind
        real(r_8), intent(in) :: heart_in_ind  
        real(r_8), intent(in) :: bminc_in_ind
        real(r_8), intent(in) :: height


        real(r_8), intent(out) :: leaf_inc_alloc
        real(r_8), intent(out) :: root_inc_alloc
        real(r_8), intent(out) :: sap_inc_alloc
        real(r_8), intent(out) :: heart_inc_alloc


        !initialize variables
        leaf_inc_alloc = 0.0D0
        root_inc_alloc = 0.0D0
        sap_inc_alloc = 0.0D0
        heart_inc_alloc = 0.0D0

        leaf_inc_alloc = ( bminc_in_ind - leaf_in_ind / ltor + root_in_ind )/ (1. + 1./ ltor)


        if (leaf_inc_alloc.gt.0.) then
        
            !Positive allocation to leafmass
            
            root_inc_alloc = bminc_in_ind - leaf_inc_alloc  !eqn (31)


           !Add killed roots (if any) to below-ground litter - TO BE DONE

            if(root_inc_alloc.lt.0.)then
                
                leaf_inc_alloc = bminc_in_ind

                root_inc_alloc = (leaf_in_ind + leaf_inc_alloc) / ltor - root_in_ind
                ! print*, 'ATTENTION PLEASE!!!!!!!!!!!'
            endif 
        else 

            !Negative allocation to leaf mass

            root_inc_alloc = bminc_in_ind
            
            leaf_inc_alloc = (root_in_ind + root_inc_alloc) * ltor - leaf_in_ind  !from eqn (9)

            ! print*, 'ATTENTION PLEASE!!!!!!!!!!!'

        endif

        ! !Calculate sminc_ind (must be negative)
        sap_inc_alloc = ((leaf_in_ind + leaf_inc_alloc) * sla) / klatosa*dwood*height - sap_in_ind
        ! print*, 'sap inc alloc mus be negative', sap_inc_alloc

        heart_inc_alloc = heart_in_ind + abs(sap_inc_alloc)
    end subroutine

    subroutine mortality_turnover (leaf_in_ind, root_in_ind, sap_in_ind, heart_in_ind,storage_in_ind,&
        leaf_turn, root_turn, sap_turn, heart_turn, storage_turn)
        !ATENÇÃO: 1 ha
        
        !C in compartments previous the allocation
        real(r_8), intent(in) :: leaf_in_ind
        real(r_8), intent(in) :: sap_in_ind
        real(r_8), intent(in) :: root_in_ind
        real(r_8), intent(in) :: heart_in_ind
        real(r_8), intent(in) :: storage_in_ind

        !gC/m2
        real(r_8), intent(out) :: leaf_turn !amount of C to be lost by turnover
        real(r_8), intent(out) :: root_turn !amount of C to be lost by turnover
        real(r_8), intent(out) :: sap_turn !amount of C to be lost by turnover
        real(r_8), intent(out) :: heart_turn !amount of C to be lost by turnover
        real(r_8), intent(out) :: storage_turn !amount of C to be lost by turnover



        leaf_turn = leaf_in_ind*leaf_turnover

        root_turn = root_in_ind*root_turnover

        sap_turn = sap_in_ind*sap_turnover

        storage_turn = storage_in_ind*(1/2) !provisory

        !heartwood incorporates the dead tissue from sapwood
        heart_turn = (heart_in_ind*heart_turnover) + sap_turn
        


    end subroutine

    subroutine storage_accumulation (bminc_in_ind, storage_inc_alloc)
        !ATENÇÃO: 1 ha
        
        !C in compartments previous the allocation
        real(r_8), intent(in) :: bminc_in_ind !non allocated NPP

        !gC/m2
        real(r_8), intent(out) :: storage_inc_alloc !carbon to storage


        storage_inc_alloc = bminc_in_ind
        
    end subroutine

    ! subroutine reallocation(bminc_in_ind, leaf_in_ind, root_in_ind, sap_in_ind,heart_in_ind, height,storage_in_ind,&
    !     leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc,storage_inc_alloc)
        
        
    !     real(r_8), intent(in) :: leaf_in_ind 
    !     real(r_8), intent(in) :: sap_in_ind
    !     real(r_8), intent(in) :: root_in_ind
    !     real(r_8), intent(in) :: heart_in_ind
    !     real(r_8), intent(in) :: storage_in_ind  
    !     real(r_8), intent(in) :: bminc_in_ind
    !     real(r_8), intent(in) :: height


    !     real(r_8), intent(out) :: leaf_inc_alloc
    !     real(r_8), intent(out) :: root_inc_alloc
    !     real(r_8), intent(out) :: sap_inc_alloc
    !     real(r_8), intent(out) :: heart_inc_alloc
    !     real(r_8), intent(out) :: storage_inc_alloc

    !     real(r_8) :: c_available

    !     !initialize variables
    !     leaf_inc_alloc = 0.0D0
    !     root_inc_alloc = 0.0D0
    !     sap_inc_alloc = 0.0D0
    !     heart_inc_alloc = 0.0D0
    !     storage_inc_alloc = 0.0D0
    !     c_available = 0.0D0

    !     leaf_inc_alloc = ( c_available - leaf_in_ind / ltor + root_in_ind )/ (1. + 1./ ltor)


    !     if (leaf_inc_alloc.gt.0.) then
        
    !         !Positive allocation to leafmass
            
    !         root_inc_alloc = c_available - leaf_inc_alloc  !eqn (31)


    !        !Add killed roots (if any) to below-ground litter - TO BE DONE

    !         if(root_inc_alloc.lt.0.)then
                
    !             leaf_inc_alloc = c_available

    !             root_inc_alloc = (leaf_in_ind + leaf_inc_alloc) / ltor - root_in_ind
    !             ! print*, 'ATTENTION PLEASE!!!!!!!!!!!'
    !         endif 
    !     else 

    !         !Negative allocation to leaf mass

    !         root_inc_alloc = c_available
            
    !         leaf_inc_alloc = (root_in_ind + root_inc_alloc) * ltor - leaf_in_ind  !from eqn (9)

    !         ! print*, 'ATTENTION PLEASE!!!!!!!!!!!'

    !     endif

    !     storage_inc_alloc = -(leaf_inc_alloc + root_inc_alloc)
    ! end subroutine

    subroutine reallocation (storage_in_ind,bminc_in_ind, leaf_inc_min, root_inc_min,&
        leaf_inc_alloc, root_inc_alloc, sap_inc_alloc, heart_inc_alloc, storage_inc_alloc)
        
        !here for reallocation we sum the C available from NPP and storage to reallocate (only for leaves and fine roots)


        !inputs
        real(r_8), intent(in) :: storage_in_ind
        real(r_8), intent(in) :: leaf_inc_min
        real(r_8), intent(in) :: root_inc_min
        real(r_8), intent(in) :: bminc_in_ind

        !outputs
        real(r_8), intent(out) :: leaf_inc_alloc
        real(r_8), intent(out) :: root_inc_alloc
        real(r_8), intent(out) :: sap_inc_alloc
        real(r_8), intent(out) :: heart_inc_alloc
        real(r_8), intent(out) :: storage_inc_alloc

        
        !internal
        real(r_8) :: c_available !C from NPP + storage

        !initialize variables
        leaf_inc_alloc = 0.0D0
        root_inc_alloc = 0.0D0
        sap_inc_alloc = 0.0D0
        heart_inc_alloc = 0.0D0
        storage_inc_alloc = 0.0D0
        c_available = 0.0D0


        c_available = storage_in_ind + bminc_in_ind

        leaf_inc_alloc = leaf_inc_min

        root_inc_alloc = root_inc_min

        storage_inc_alloc = c_available - (leaf_inc_alloc + root_inc_alloc)
        
    end subroutine
    
    



end module allocation


