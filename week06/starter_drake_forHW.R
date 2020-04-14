# Drake Equation starter file

# Fill in your parameters:
Rstar =  # 1-100 Msun/yr, SFR of MW
fp =  # 0.1 - 1, fraction of stars with planetary systems
ne =  # 0.1 - 1, fraction of planets that have ecosystem per system
fl =  # mean = 0.5, median = 0.63, fraction of planets that have life
fi =  # 0.001-1, fraction of life that is intellegent
fc =  # 0.01 - 1, fraction of intellegent life that has IS comms
L =  # 100- 10,000,000,000 in years, average length of civiliaztion's comms
n =  # number of planets SETI observs
N_mw = 250*1e9 # number of stars in MW

# Pick the number of found civilizations we want
Nmax =  # number of planets we want to observe

# with above, N_et = 2.5 civiliations in our MW
N_et = Rstar*fp*ne*fl*fi*fc*L
p = N_et/N_mw
# ...

