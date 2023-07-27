import matplotlib.pyplot as plt
import module_006 as my_module

# Condições iniciais
leaf_in = 0.01
root_in = 0.01
sap_in = 15.
heart_in = 40.
dens_in = 1.
bminc_in = 0.8
storage_in = 0.5

# Número de passos de tempo
num_steps = 100

# Listas para armazenar os resultados de cada passo de tempo
leaf_results = []
root_results = []
sap_results = []
heart_results = []
storage_results = []

# Loop para iterar nos passos de tempo
for step in range(num_steps):
    # Executa a função alloc com as condições iniciais
    res = my_module.allocation.alloc(leaf_in, root_in, sap_in, heart_in, storage_in, bminc_in, dens_in)
    
    # Desempacota os resultados em variáveis individuais
    leaf_out, root_out, sap_out, heart_out, storage_out = res

    # Retroalimentação dos resultados para o próximo passo de tempo
    leaf_in = leaf_out
    root_in = root_out
    sap_in = sap_out
    heart_in = heart_out
    storage_in = storage_out

    # Armazena os resultados deste passo de tempo nas listas
    leaf_results.append(leaf_out)
    root_results.append(root_out)
    sap_results.append(sap_out)
    heart_results.append(heart_out)
    storage_results.append(storage_out)

# Plotagem dos resultados
plt.plot(leaf_results, label='Leaf')
plt.plot(root_results, label='Root')
plt.plot(sap_results, label='Sap')
plt.plot(heart_results, label='Heart')
plt.plot(storage_results, label='Storage')
plt.legend()
plt.xlabel('Time Step')
plt.ylabel('Value')
plt.show()

# import matplotlib.pyplot as plt
# import module_006 as my_module
# 
# 
# leaf_in = 1.5 #0.0001
# root_in = 0.8 #0.0001
# sap_in = 29.
# heart_in = 108.
# dens_in = 3    
# bminc_in = 4.5
# storage_in = 1.

# print(bminc_in)
# 
# res = my_module.allocation.alloc(leaf_in, root_in, sap_in, heart_in, storage_in, bminc_in, dens_in)
# 
# leaf_out, root_out, sap_out, heart_out, storage_out = res
# 
# sample1 = []
# sample2 = []
# x = 0
# tmp = 0
# 
# while True:
# 
    # if x > 3: break

    # res = my_module.allocation.alloc(leaf_in, root_in, sap_in, heart_in, bminc_in,dens_in)

    # res = my_module.all
# 
    # r1 = sap_in
    # r2 = leaf_in
    # leaf_in, root_in, sap_in, heart_in = res
    # sample1.append(r1)
    # sample2.append(r2)
    # x += 1

    # print(r1)

# plt.plot(sample1)
# plt.plot(sample2)

# plt.savefig("test_module006.png")
# plt.clf() # limpa o grfico

# assessing a variable:
# t = my_module.program2

# print(t)



# p1 = 100000 # kg/m2
# p2 = 300000


# sample = []
# x = 0
# tmp = 0


# while True:
    
    # if x > 3: break
    
    # chama a funcao e guarda os outputs na variavel res
    # res = my_module.establishment.establish(p1,p2) #colocar só os inputs

    # r1 = res
    # sample.append(r1)
    # x += 1
    # p1 += 250
        
# plt.plot(sample)
# plt.savefig("test_module006.png")
# plt.clf() # limpa o grfico
 
