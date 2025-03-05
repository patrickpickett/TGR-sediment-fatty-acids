# TGR-sediment-fatty-acids
This repository contains data and scripts relevant to the manuscript 'Seasonal patterns and phytoplankton blooms influence sediment FA composition in a large reservoir'. Description of data columns are in the sections below.

#### TGR_phytoplankton_2021.csv” contains phytoplankton data from Pengxi and Modao Rivers in the Three Gorges Reservoir, China, collected in 2021. The data includes three sampling periods: a single event in Winter and Summer, and daily samples for 7 days during Spring, which coincided with the peak of the phytoplankton bloom. The data is organized by river (PX and MD), with multiple sites (e.g., PX1, PX2, MD1, MD2) and locations within each site.
- year = observation year (2021)
- month = 3 levels: March, May and September
- season = 3 levels: winter, spring and summer
- date = (dd/mm/yyyy format)
- river = 2 rivers including Pengxi (PX) and Modao (MD)
- site = PX1-PX7 and MD1-MD4
- month-site = Identifier combining month (in numeric value) and site (e.g., 9-PX-1)
- category = Phytoplankton and microalgae phylum 
- name = Common name of the phytoplankton species
- latin name = Scientific name of species
- biomass = Biomass of the phytoplankton species (in mg/L)
- amount = Count of the phytoplankton species observed
- total_biomass = Total biomass calculated for the species (biomass * amount)
- total_biomass_mgL = Total biomass expressed in milligrams per liter (mg/L)
- density_individualL = Density of individuals per liter (individuals/L)
