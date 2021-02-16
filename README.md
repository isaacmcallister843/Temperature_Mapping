# Temperature_Mapping

## Motivation 
Mining companies have an interest in understanding the climate and enviroment of the area they are operating in. A data driven view of the study area allows companies to understand their impact on the land and best pratices to mitigate it. For this project we had developed a dataset of timeseries temperature data using 32 monitoring stations, they had been operating across most of 2019 and 2020. Using a KNN algorithem the temperature was predicted for the entire study area. We also compared the predicted data to high res infrared photos from satallites, to validate and correct for error. This project was a proof of concept, and the tools developed here would be applied in many more projects. 

## Intellectual Property
Since I worked in a team to develop these products for large clients I do not have permission to distribute the raw data used in these projects. I have also remnamed any identifing files and removed the location data from the showcased images.

## Data and Modeling
The data was collected by external teams and was presented as a csv and after proccesing was in the following columns: 

|Time|Easting|Northing|Temperature Location 1 (C)|Temperature Location 2 (C)|.......|Temperature Location 32 (C)|
|----|-------|--------|--------------------------|--------------------------|-------|---------------------------|

Each row could be accessed by the KNN algorithem and create a heat map for a desired time.

## Features 
- Fast and easy heatmap generation for time series data
- Raster and JPG output 
- Batch proccesing for large dataframes 

## Code Examples

Once the dataframe is generated from the raw data the following lines are all that is needed to great a heatmap. The dataframe "points" is the easting and northing values of each location. 
```R
generate_heat_map(merged_frame, points,"2019-09-07")
```








