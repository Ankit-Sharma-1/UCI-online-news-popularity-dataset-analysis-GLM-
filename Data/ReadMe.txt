Data released: 19 May 2017
Data created by: Dr. Jiwon Kim, School of Civil Enginnering (jiwon.kim@uq.edu.au)
Data sent to: Dr. Ian Wood, School of Mathematics & Physics (iwood@maths.uq.edu.au)

The data descbied below are intended to be used for course DATA7202.
Please contact Jiwon Kim (jiwon.kim@uq.edu.au) to use the data in other courses or research.

This folder contains the following data files:
- DATA7202_cell_info_2km.csv
- DATA7202_cell_info_2km_1.PNG
- DATA7202_cell_info_2km_2.PNG
- DATA7202_crossBoundaryFlow_20130225-20130303.csv
- DATA7202_crossBoundaryFlow_20130304-20130310.csv
- DATA7202_crossBoundaryFlow_20130311-20130317.csv
- DATA7202_crossBoundaryFlow_20130318-20130324.csv

"DATA7202_cell_info_2km.csv" provides the coordiates of cell centroids.

"DATA7202_cell_info_2km_~.PNG" shows locations and boundaries of cells on the map.

"DATA7202_crossBoundaryFlow_~.csv" files contain data for cross-boundary flows (i.e., flow between adjacent cells) of bus passengers, where the magnitudes of flow represent approximately 50% of go card bus passengers.
Each file contains the following data columns:
- date : observation date in the format of yyyy-MM-dd
- time_id : integer index for observation time window
- start_time : start time of the observation time window, in total minutes since the start of the day.
- end_time : end time of the observation time window, in total minutes since the start of the day.
- source_cell : start cell of the cross-boundary flow
- target_cell : end cell of the cross boundary flow
- num_traj : cross-boundary flow (from source_cell to target_cell) during the observation time window, i.e., the number of trajectories that cross the boundary between source_cell and target_cell during the current time window [start_time, end_time]
