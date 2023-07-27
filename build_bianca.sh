#/bin/sh
 
pyenv local 3.10.6


rm -rf *.mod
rm -rf *.pyf
rm -rf *.png


# Esse primeiro comando cria o arquivo interf.pyf
python -m numpy.f2py -h interf.pyf parameters.f90 mod_allometry.f90 mod_gc_occupation.f90 mod_allocation.f90 main_program.f90 -m module_006 --overwrite-signature

# Este comando usa o interf.pyf e o codigo para gerar o módulo
python -m numpy.f2py interf.pyf -c parameters.f90 mod_allometry.f90 mod_gc_occupation.f90 mod_allocation.f90 main_program.f90


