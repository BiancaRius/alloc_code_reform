import matplotlib.pyplot as plt
import module_006 as my_module


leaf_in = 1.5 #0.0001
root_in = 0.8 #0.0001
sap_in = 29.
heart_in = 108.
dens_in = 3    
bminc_in = 4.5

sample1 = []
sample2 = []
x = 0
tmp = 0

while True:

    if x > 3: break

    res = my_module.allocation.alloc(leaf_in, root_in, sap_in, heart_in, bminc_in,dens_in)

    r1 = sap_in
    r2 = leaf_in
    leaf_in, root_in, sap_in, heart_in = res
    sample1.append(r1)
    sample2.append(r2)
    x += 1

    # print(r1)

# plt.plot(sample1)
plt.plot(sample2)

plt.savefig("test_module006.png")
plt.clf() # limpa o grfico

#assessing a variable:
#t = my_module.program2

#print(t)

# #

# p1 = 100000 # kg/m2
# p2 = 300000


# sample = []
# x = 0
# tmp = 0


# while True:
    
#     if x > 3: break
    
#     # chama a funcao e guarda os outputs na variavel res
#     res = my_module.establishment.establish(p1,p2) #colocar só os inputs

#     r1 = res
#     sample.append(r1)
#     x += 1
#     p1 += 250
        
# plt.plot(sample)
# plt.savefig("test_module006.png")
# plt.clf() # limpa o grfico
 
