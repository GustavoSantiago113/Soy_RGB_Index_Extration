# Reflectance analysis of vegetative development of soy irrigated with slaughterhouse-treated effluent

## Contents

1. [Abstract](#abstract)
2. [Sites and Data collection](#sites-and-data-collection)
3. [Image analysis](#image-analysis)
4. [Data analysis]()


## Abstract

The link between wastewater reuse and sustainable agriculture emphasizes the significance of embracing innovative methods and technologies. Computer vision and digital image techniques, when combined with existing approaches such as vegetation indices and chlorophyll analysis, emerge as powerful instruments for plant nutritional monitoring and growth. The objective of this study was to analyze, by reflectance, the vegetative development of soybeans irrigated with treated slaughterhouse effluent. Also, the minor objectives were to i) monitor soybean development using active and passive optical sensors by determining vegetative indices; ii) develop an algorithm for image segmentation and calculation of vegetation indices, iii) correlate and generate linear models associating the visible vegetative indices with relative values of chlorophyll and NDVI index. This study was performed at an experimental area annex to a slaughterhouse at Pirassununga-Brazil. The effluent was treated and used to irrigate soybeans with concentrations of 0, 25, 50, 75, and 100% under the presence or absence of inoculation. Data was collected with a digital camera, a chlorophyll meter, and a GreenSeeker during its vegetative development. To extract the vegetation indices from the images, a Python script was developed. The indices investigated in this study presented different behaviors throughout the soybean vegetative development, with significative differences (p<0.05) in the days after sowing, treatment, inoculation, and the interaction between them. This represents the first documented attempt to analyze vegetation indices obtained from images taken using a digital camera at a field scale of a crop irrigated with treated wastewater.

## Sites and Data collection

The study was developed in an experimental area located on the USP Campus “Fernando Costa”, in Pirassununga, Sao Paulo State, Brazil, in an area adjacent to the school slaughterhouse. 

The experimental design was in randomized blocks with five treatments and four replications, totaling twenty plots. The treatments consisted of dosages of treated slaughterhouse effluent (ETA), being: (100E) 100% ETA, (75E) 75% ETA, (50E) 50% ETA, (25E) 25% ETA and (0E) 0% ETA, with complementary irrigation, in each treatment, with water.Each plot was divided into inoculated and not inoculated soybeans, generating subplots. To sow half of the plot, Bradyrhizobium japonicum was inoculated in the seed. Resuming, each experimental soybean plot was composed of 14 lines, spaced 50 cm apart, and 7 meters long. The useful plot was 5 lines per subplot, considering the central lines of the plot.

The slaughterhouse effluent used for irrigation came from the School Slaughterhouse on the USP “Fernando Costa” campus, Pirassununga, Sao Paulo State, Brazil, and treated using a UASB reactor (Upflow Anaerobic Sludge Blanket).

To acquire the images, a digital camera model Canon Eos Rebel T6I, with 12MP was used. The camera was supported perpendicularly to the ground on a tripod 100 cm high from the ground in the first three collections and 160 cm high from the ground during the final four collections with a white object in the image acquisition field, aiming for color balance. In the situations when direct sunlight was on the canopy, an object was used to generate shadow in the area of the canopy, and a camera to avoid different illumination conditions within the image collections of the study.

The method to obtain active reflectance values of the canopy was using the GreenSeeker® optical sensor. The active chlorophyll meter ClorofiLog from the company Falker to obtain the relative content of chlorophyll.

## Image analysis

The [script](Algorithm.ipynb) used the library OpenCV to resize (50% of the original size), extract the background of the image, obtain the mean values of R, G, and B, and then calculate the indices. To extract the background, the image was converted to HSV space color and then employed a mask in the image considering the yellow and green hues.

From the segmented images were calculated the visible vegetative indices Modified Photochemical Reflectance Index (MPRI), Dark Green Color Index (DGCI), normalized R, G e B, and normalized H, S and V.

To calculate the indices, was developed another Python [algorithm](Algorithm.ipynb) by the authors, available in this open GitHub repository. In the algorithm, using the OpenCV library, the B, G, and R channels were extracted using the function “split”. Then, the mean values of all the channels were calculated and divided by 255. With the new values of each channel, the H, S, and V indices were calculated, followed by the MPRI and DGCI indices using the aforementioned formulas.

## Data analysis

The statistical analysis was conducted using an [R algorithm](DataAnalysis.R) developed by the authors. The main packages used to perform the analysis were tidyverse, Metrics, and multicompview. Firstly, were performed an Anova analysis for all the variables, considering block, treatment, inoculation, and DAS as factors and the interaction between those factors. Lastly, linear models to relate the active vegetation indices and the passive were performed.


