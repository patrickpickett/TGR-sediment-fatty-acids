# TGR-sediment-fatty-acids
This repository contains data and scripts relevant to the manuscript 'Seasonal patterns and phytoplankton blooms influence sediment FA composition in a large reservoir'. Description of data columns are in the sections below.
# Data sets #
Variables common across data sets:
- year = observation year
- month = 3 levels: March, May and September
- season = 3 levels: winter, spring and summer
- date = (dd/mm/yyyy format)
- river = 2 rivers including Pengxi (PX) and Modao (MD)
- site = (e.g., PX4, MD4 etc.)
- ID = Identifier combining month (in numeric value) and site (e.g., 9-PX-1)

#### ‘TGR_phytoplankton_2021.csv’ - #### contains phytoplankton data from Pengxi and Modao Rivers in the Three Gorges Reservoir, China, collected in 2021. The data includes three sampling periods: a single event in Winter and Summer, and daily samples for 7 days during Spring, which coincided with the peak of the phytoplankton bloom. 
Variable specific to data set:
- category = Phytoplankton and microalgae phylum
- name = Common name of the phytoplankton species
- latin name = Scientific name of species
- biomass = Biomass of the phytoplankton species (in mg/L)
- amount = Count of the phytoplankton species observed
- total_biomass = Total biomass calculated for the species (biomass * amount)
- total_biomass_mgL = Total biomass expressed in milligrams per liter (mg/L)
- density_individualL = Density of individuals per liter (individuals/L)

#### ‘TGR_SI_2021_22.csv’ - contains stable isotope data from Pengxi and Modao Rivers in the Three Gorges Reservoir, China, collected in 2021 and 2022. The data includes three sampling periods: a single event in Winter and Summer, and daily samples for 7 days during Spring, which coincided with the peak of the phytoplankton bloom. 
Variable specific to data set:
-	sample = Sample type. Includes: sediment, riparian vegetation, surface POM, benthic POM, zooplankton.	
-	d13C = Ratio of stable isotopes Carbon-13 to Carbon-12
-	C% = Percentage of carbon in the sample.
-	C13/C12 = ratio of Carbon-13 to Carbon-12 isotopes	
-	d15N = Ratio of stable isotopes Nitrogen-15 to Nitrogen-14	
-	N% = Percentage of nitrogen in the sample
-	N15/N14 = ratio of Nitrogen-15 to Nitrogen-14 isotopes	

#### ‘TGR_sediment_FA_2021.csv’ - contains sediment fatty acid data collected in 2021 from six sites in the Pengxi and Modao Rivers of the Three Gorges Reservoir, China (PX4-PX6, MD2-MD3). Fatty acids are labelled using standard nomenclature.
-	full_ID = Unique identifier for each sample (includes full date, site and river)
-	sample = Type of sample (sediment)
-	type = Type of measurement (relative or absolute)
-	Fatty Acid Columns = Various fatty acid measurements (e.g., 10_0, 12_0, 13_0, etc.)

#### TGR_surface_benthic_water_quality_2021.csv’ - contains water quality and nutrient data collected in 2021 from six sites in the Pengxi and Modao Rivers of the Three Gorges Reservoir, China (PX4-PX6, MD2-MD3). Measurements were taken from the surface (0.5 m below the surface) and benthic (0.5 m above the bottom) zones. Note: "Chla_single" refers to samples processed in the lab, while "Chlorophyll" refers to measurements taken in situ using the data logger.
-	Layer = Includes benthic or surface 
-	Location = Alternate identifier of layer
-	TP = Total phosphorus (mg L-1)
-	DTP = Dissolved total phosphorus (mg L-1)
-	TN = Total nitrogen (mg L-1)
-	DTN = Dissolved total nitrogen (mg L-1)
-	Chla_single = Chlorophyll a (µg L-1)
-	Conductivity = (S cm-1)
-	conductivity_spc = Specific conductivity (S/m) 
-	Temp = (°C)
-	Pressure = (dBar)
-	Chlorophyll = (µg L-1)
-	DO_sat = Dissolved oxygen saturation (%)
-	PAR = Photosynthetically active radiation (µmol/m²/s)
-	pH =
-	depth = (m)
-	salinity = (psu)
-	density_anomaly = (kg/m3)
-	DO_conc = dissolved oxygen (mg L-1)
-	absolute_depth = total depth of water column (m)
-	velocity_ms = (m s-1)

#### TGR_ water_quality_profiles_2021.csv’ - contains water quality profiles 2021 from six sites in the Pengxi and Modao Rivers of the Three Gorges Reservoir, China (PX4-PX6, MD2-MD3). Profiles were measured using a RBRmaestro3 data logger. 
-	Conductivity = (S cm-1)
-	temp = (°C)
-	pressure = (dBar)
-	chlorophyll = (µg L-1)
-	PAR = Photosynthetically active radiation (µmol/m²/s)
-	pH =
-	depth = (m)
-	salinity = (psu)
-	conductivity_spc = Specific conductivity (S/m)
-	density_anomaly = (kg/m3)
-	DO_conc = dissolved oxygen (mg L-1)	
