Code for allocation logic reformulated

!!!!!!!!!!!! - Running the code  - !!!!!!!!!!!!  

**1st option (using python)

    Loop to update variables in run_modules.py

    Value for initial C is in run_modules.py

			./building_bianca.sh


			python run_modules.py





**2nd option - using only fortran

    it is necessary to add a loop in the file program_bia.f90 (it is commented in the code)

    value for initial C is in program_bia.f90


			gfortran constants.f90 allocation_reform_bia.f90 program_bia.f90 -o a.exe


			./a.exe




!!!!!!!!!!!! - Files description - !!!!!!!!!!!! 

			allocation_reform_bia.90 --> main file (contains all functions and subroutines)


			constants.f90 --> file with constants used in main file


			buinding_bianca.sh --> creates module to be used in python with f2py


			module_006.cpython-37m-x86_64-linux-gnu.so --> module


			run_modules.py --> use the module to initiate and update variables. Can be used to generate graphs


