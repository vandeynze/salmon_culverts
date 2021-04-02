Each module accomplishes a subsequent step in analysis.  

**A.pnshpexplore:** Initial cleaning and draft figures for all PNSHP data. Saves out `/output/PNSHP_full_working.csv` and `/output/PNSHP_prj_working.csv`.  
**B.culvertsexplore:** Filtering, cleaning, and inital exploration of fish passage projects in the PNSHP data. Saves out figures (`/output/maps/culverts/fig_map_*.png`) and data, including the "full" data used in ArcGIS processing (`/output/culverts_full_mapping.csv`) and further analysis (`/output/culverts_full_working.csv`, `/output/culverts_wrk_working.csv`, and `/output/culverts_prj_working.csv`).  
**C.culvertsspatial:** Merges culvert worksites with various geospatial data and saves out data for further analysis (most importantly `/output/culverts_pure_modelling.csv` and `output/culverts_pure_spatial.csv`).  
**D.culvertcosts:** Fits predictive and inferential models of culvert costs based on predictors from geospatial datalayers.
**E.optimization:** Solvers for linear and non-linear multiobjective optimization framework.  
**F.culvertinventories:** Explores state culvert inventories and other spatial data used in applications.  