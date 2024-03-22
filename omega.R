# Title: Omega
# Author: Josh Langfus

# Purpose: This script contains functions useful for computing McDonald's omega statistics (Ravelle & Condon, 2016)

require(EFAtools)
require(lavaan)

# m must be a bifactor model fit with lavaan. the g_name argument must match the name of the general factor in this model
o <- OMEGA(model = m, g_name = "g")

