This readme file was generated on 2026-04-02 by Mackenzie Thomson

GENERAL INFORMATION

1. Title of Dataset: Solar Farm Site Suitability and Energy Potential Analysis in British Columbia: A GIS-AHP Approach 

2. Author Information: 

	A. Principal Investigator Contact Information
		Name: Mackenzie Thomson
		Institution: University of British Columbia
		Email: mack.thom26@gmail.com

3. Years of data collection: 1999-01-01 to 2024-12-31 (Note: Solar radiation data represent long-term modeled averages within this window)

4. Geographic location of data collection: British Columbia, Canada 
	West = -139.051416
	East = -114.054375
	North = 60.000065
	South = 48.308613

5. Description of Dataset: This dataset explores the most optimal locations for utility-scale solar farm development and models the gross energy potential across the most suitable sites in British Columbia. 

6. License: Creative Commons Attribution 4.0 International (CC-BY 4.0) 

SHARING/ACCESS INFORMATION

1. Data Sources 
	A. Global Solar Atlas: https://globalsolaratlas.info/download
	B. BC Data Catalogue: https://catalogue.data.gov.bc.ca/dataset/digital-elevation-model-for-british-columbia-cded-1-250-000
	C. Statistics Canada: https://www150.statcan.gc.ca/n1/daily-quotidien/250327/dq250327d-eng.htm
	D. National Forestry Information Service: https://opendata.nfis.org/mapserver/nfis-change_eng.html
	E. Environment and Climate Change Canada: https://catalogue.ec.gc.ca/geonetwork/srv/api/records/6c343726-1e92-451a-876a-76e17d398a1c
	F. GADM: https://gadm.org/data.html
	G. BC Data Catalogue: https://catalogue.data.gov.bc.ca/dataset/7-5m-major-roads-the-atlas-of-canada-base-maps-for-bc
	H. BC Data Catalogue: https://catalogue.data.gov.bc.ca/dataset/7-5m-major-cities-the-atlas-of-canada-base-maps-of-bc
	I. BC Data Catalogue: https://catalogue.data.gov.bc.ca/dataset/ecoprovinces-ecoregion-ecosystem-classification-of-british-columbia
	J. Future Energy Systems: https://www.futureenergysystems.ca/resources/renewable-energy-projects-canada
	K. Integrated Cadastral Information Society (ICIS): Infrastructure data (Restricted access layer) -https://www.icisociety.ca/ 

DATA & FILE OVERVIEW

1. File List: 
	MThomson_SolarSuitability.pdf (Final Research Report) 
	README.txt (This file) 
	CapstonePoster_MThomson.pdf (Research Project Poster) 
	|
	+---Data
		|
		+---AHP 
		|   aspect_reclassified.tif 
		|   cities_reclassified.tif
		|   ghi_reclassified.tif
		|   roads_reclassified.tif
		|   slope_reclassified.tif
		|   substations_reclassified.tif
		|   pvout_clean.tif
		+---Mask
		|   alr_binary_3005.tif
		|   canopy_binary_3005.tif
		|   cities_binary_3005.tif
		|   lcr_binary_3005.tif
		|   pca_binary_3005.tif
		|   roads_binary_3005.tif
		|   final_mask.tif
		+---Script
		|   SolarSuitability_script.R
		+---SuitabilitySurfaces
		|   FinalSuitability.tif
		|   suitability_raw.tif

METHODOLOGICAL INFORMATION

1. Description of methods used for collection/generation of data: 
	A Multi-Criteria Decision Analysis (MCDA) was conducted using the Analytic Hierarchy Process (AHP) to weight and combine spatial criteria for solar farm 	site suitability. Full methodology, criteria justification, and weight derivation are detailed in the accompanying report: MThomson_SolarSuitability.pdf. 

DATA-SPECIFIC INFORMATION FOR:  

	1. /AHP: Contains the standardized and reclassified criteria rasters used in the weighted overlay. The scale is as followed: 1 = least suitable, 2 = less 	suitable, 3 = moderately suitable, 4 = suitable, 5 = very suitable. 
		a. /aspect_reclassified.tif: raster layer representing aspect in degrees.
			1 = 315 - 45 (North)
			3 = 45 - 135 (East)
			3 = 225 - 315 (West)
			5 = 135 - 225 (South) 
		b. /cities_reclassified.tif: raster layer representing proximity to cities in meters. 
			1 = >45, 000
			2 = 30,000 - 45,000
			3 = 15,000 - 30,000
			4 = 5000 - 15,000
			5 = 1000 - 5000 
		c. /ghi_reclassified.tif: raster layer representing solar resource in (kWh/m²).
			1 = <600
			2 = 600 - 800
			3 = 800 - 1000
			4 = 1000 - 1200
			5 = >1200
		d. /roads_reclassified.tif: raster layer representing proximity to roads in meters. 
			1 = >30,000
			2 = 15,000 - 30,000
			3 = 5000 - 15000
			4 = 1000 - 5000
			5 = 100 - 1000
		e. /slope_reclassified.tif: raster layer representing slope in degrees.
			1 = >15
			2 = 10 - 15 
			3 = 5 - 10 
			4 = 2 - 5
			5 = 0 - 2 
		f. /substations_reclassified.tif: raster layer representing proximity to substations in meters. 
			1 = >6000
			2 = 3000 - 6000
			3 = 1500 - 3000
			4 = 500 - 1500
			5 = 0 - 500 
			
		g. /pvout_clean.tif: raster layer representing PVOUT values in kWh/kWp/year. 

	2. /Mask: Contains the binary constraint rasters used to exclude restricted lands. The scale is as followed: 0 = restricted, 1 = acceptable. 
		a. alr_binary_3005.tif
			0 (restricted) = within Agricultural Land Reserve 
			1 (acceptable) = not within Agricultural Land Reserve 
		b. canopy_binary_3005.tif
			0 (restricted) = canopy density > 40 %
			1 (acceptable) = canopy density <= 40 %
		c. cities_binary_3005.tif
			0 (restricted) = within 1000 m city buffer
			1 (acceptable) = not within 1000 m city buffer 
		d. lcr_binary_3005.tif
			0 (restricted) = unsuitable land cover (Built up and artificial surfaces; Inland water body; Treed wetland; Wetland (non-treed); Permanent snow and ice
			1 (acceptable) = Cropland; Treed; Treed area disturbance; Grassland and shrubland; Sparsely vegetated land; Barren land
		e. pca_binary_3005.tif
			0 (restricted) = within a Protected and Conserved area 
			1 (acceptable) = not within a Protected and Conserved area
		f. roads_binary_3005.tif
			0 (restricted) = within 100 m road buffer
			1 (acceptable) = not within 100 m road buffer 

	3. /Script: Contains a simplified version of the R script used for analysis and geoprocessing - all data preprocessing steps have been excluded.  

	4. /SuitabilitySurfaces: Contains both the unmasked (suitability_raw.tif) and masked (FinalSuitability.tif) output surfaces from the AHP-derived weighted overlay.

Note: All data has been projected to BC Environmental Albers (EPSG:3005) and clipped to the provinces official administrative boundary. 

SOFTWARE & TECHNOLOGY

1. Primary Software:
	R (Version 4.5.0): Used as the primary environment for spatial data processing and spatial analysis.
	ArcGIS Pro (Version 3.5.2): Used for data visualization.


METHODOLOGICAL INFORMATION

1. Instrument- or software-specific information needed to interpret the data: 
ArcGIS Pro, Version 
RStudio, Version 



