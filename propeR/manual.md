# propeR manual

<p align="center"><img align="center" src="meta/logo/propeR_logo_v1.png" width="200px"></p>

## Contents


* [Introduction and Preprocessing GTFS feed](#introduction-and-preprocessing-gtfs-feed)
  	* [Background and Software Prerequisities](#background-and-software-prerequisites)
  	* [Creating a GTFS feed](#creating-a-gtfs-feed)
  	* [TransXChange to GTFS](#transxchange-to-gtfs)
  		* [TransXChange2GTFS by danbillingsley](transxchange2gtfs-by-danbillingsley)
  		* [transxchange2gtfs by planar network](transxchange2gtfs-by-planar-network)
  	* [CIF to GTFS](#cif-to-gtfs)
  	* [Cleaning the GTFS Data](#cleaning-the-gtfs-data)
  	* [Sample GTFS Data](#sample-gtfs-data)  
  
* [Creating the OpenTripPlanner server](#creating-the-opentripplanner-server)    

* [Running propeR packages](#running-proper-packages)
	* [Data Prerequisites](#data-prerequisites)
	* [propeR Functions](#proper-functions)
  		* [otpConnect](#otpconnect)
  		* [importLocationData and importGeojsonData](#importlocationdata-and-importgeojsondata)
  		* [pointToPoint](#pointtopoint)
  		* [pointToPointLoop](#pointtopointloop)
  		* [pointToPointTime](#pointtopointtime)
  		* [isochrone](#isochrone)
	  	* [isochroneTime](#isochronetime)
	  	* [isochroneMulti](#isochronemulti)
	  	* [isochroneMultiIntersect](#isochronemultiintersect)
	  	* [isochroneMultiIntersectSensitivity](#isochronemultiintersectsensitivity)
	  	* [isochroneMultiIntersectTime](#isochronemultiintersecttime)
	  	* [choropleth](#choropleth)

4. [FAQ](#faq)    

## Introduction and Preprocessing GTFS feed

### Background and Software Prerequisites

* A C# compiler such as Visual Studio Code (if building own GTFS)
* MySQL (if building own GTFS)
* R and your GUI of choice, such as RStudio
* Installation of propeR and associated packages [[here]](https://github.com/datasciencecampus/access-to-services)
* Java SE Runtime Environment 8 (preferrably 64-bit) [[download here]](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)

This R package (propeR) was created to analyse multimodal transport for a number of research projects at the [Data Science Campus, Office for National Statistics](https://datasciencecampus.ons.gov.uk/). 

It was quickly realised that the universally preferred form of transport timetable data was [General Transit Feed Specification (GTFS)](https://en.wikipedia.org/wiki/General_Transit_Feed_Specification), as used by [Google maps](https://developers.google.com/transit/gtfs/reference/). However, for the UK, only [Manchester](https://transitfeeds.com/p/transport-for-greater-manchester/224) and [London](https://tfl.gov.uk/info-for/open-data-users/) have open-source GTFS feeds available. 

Understanding the complete UK transit network relies on the knowledge and software that can parse various other transit feeds such as bus data, provided in [TransXChange](https://www.gov.uk/government/collections/transxchange) format, and train data, provided in [CIF](https://www.raildeliverygroup.com/our-services/rail-data/timetable-data.html) format. 

The initial tasks was to convert these formats to GTFS. The team indentified two viable converters: (i) C# based [TransXChange2GTFS](https://github.com/danbillingsley/TransXChange2GTFS) to convert TransXChange data; and (ii) sql based [dtd2mysql](https://github.com/open-track/dtd2mysql) to convert CIF data. The [TransXChange2GTFS](https://github.com/danbillingsley/TransXChange2GTFS) code was modified by the Campus and pushed back (successfully) to the original repository. The team behind [dtd2mysql](https://github.com/open-track/dtd2mysql), [planar network](https://planar.network/), have since created their own [TransXChange to GTFS converter](https://github.com/planarnetwork/transxchange2gtfs), which does not require a C# compiler. 

Below is a more detailed set-by-step guide on how these converters are used.

### Creating a GTFS feed

A [GTFS](https://en.wikipedia.org/wiki/General_Transit_Feed_Specification) folder typically comprises the following files:

| Filename  | Description | Required? |
|---------------|-------------------------- |:---------:|
| agency.txt  | Contains information about the service operator | Yes |
| stops.txt | Contains details of each stop in the timetables provided  | Yes |
| routes.txt  | Contains information about the route  | Yes |
| trips.txt | Contains information about each trip on a route and service | Yes |
| stop_times.txt  | Contains the start and end times for stops on a journey | Yes |
| calendar.txt  | The start and end dates of journeys | Yes |
| calendar_dates.txt  | Shows exceptions for journeys for holidays etc  | Optional  |
| fare_attributes.txt | Contains information about journey fares  | Optional  |
| fare_rules.txt  | Assigns fares to certain journeys | Optional  |
| transfers.txt | Transfer type and time between stops  | Optional  |

Transport network models such as [OpenTripPlanner (OTP)](http://www.opentripplanner.org/) require a ZIP folder of these files.

#### TransXChange to GTFS

UK bus data in [TransXChange](https://www.gov.uk/government/collections/transxchange) format can be downloaded from [here](ftp://ftp.tnds.basemap.co.uk/) following the creation of an account at the Traveline website, [here](https://www.travelinedata.org.uk/traveline-open-data/traveline-national-dataset/). The data is catergorised by region. For our work, we downloaded the Wales (W) data. The data will be contained within a series of [XML](https://en.wikipedia.org/wiki/XML) files for each bus journey. For example, here is a snippet of the `CardiffBus28-CityCentre-CityCentre6_TXC_2018803-1215_CBAO028A.xml`:

```
<?xml version="1.0" encoding="utf-8"?>
<TransXChange xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xsi:schemaLocation="http://www.transxchange.org.uk/ http://www.transxchange.org.uk/schema/2.1/TransXChange_general.xsd" CreationDateTime="2018-08-03T12:15:26" ModificationDateTime="2018-08-03T12:15:26" Modification="revise" RevisionNumber="1" FileName="CardiffBus28-CityCentre-CityCentre6_TXC_2018803-1215_CBAO028A.xml" SchemaVersion="2.1" RegistrationDocument="false" xmlns="http://www.transxchange.org.uk/">
  <ServicedOrganisations>
    <ServicedOrganisation>
      <OrganisationCode>CDS</OrganisationCode>
      <Name>Cardiff</Name>
      <WorkingDays>
```

##### TransXChange2GTFS by danbillingsley

Initially, we used [TransXChange2GTFS](https://github.com/danbillingsley/TransXChange2GTFS) to convert the TransXChange files into GTFS format. TransXChange is a C# tool. Our method to convert the data was:

1. Place the XML files in the 'dir/input' folder.
2. Run the Program.cs file (i.e., `dotnet run Program.cs`).
3. The GTFS txt files will be created in the 'dir/output' folder.
4. Compress the txt files to a ZIP folder with an appropriate name (e.g., 'bus_GTFS.zip').

##### transxchange2gtfs by planar network

The team, [planar network](https://planar.network/), who we initially used to convert the UK train data to GTFS, have created a TypeScript TransXChange to GTFS converter, [transxchange2gtfs](https://github.com/planarnetwork/transxchange2gtfs). Their GitHub page provides good detailed instructions to installing and converting the files. The method we used was:

1. Install the converter as per the GitHub instructions.
2. Place the XML files in a folder.
3. Run `transxchange2gtfs *.xml gtfs-output.zip` in terminal/command line.

#### CIF to GTFS

As mentioned above, UK train data in CIF format can be downloaded from [here](http://data.atoc.org/data-download) following the creation of an account. The timetable data will download as a zipped folder named 'ttis\*\*\*.zip'. 

Inside the zipped folder will be the following files: ttfis\*\*\*.alf, ttfis\*\*\*.dat, ttfis\*\*\*.flf, ttfis\*\*\*.mca, ttfis\*\*\*.msn, ttfis\*\*\*.set, ttfis\*\*\*.tsi, and ttfis\*\*\*.ztr. Most of these files are difficult to read, hence the need for GTFS.

We used the sql tool [dtd2mysql](https://github.com/open-track/dtd2mysql) to convert the files into a SQL database, then into the GTFS format. The [dtd2mysql github](https://github.com/open-track/dtd2mysql) page gives a guide on how to convert the data. This method used here was:

1. Create a sql database with an appropriate name (e.g., 'train_database'). Note, this is easiest done under the root username with no password
2. Run the following in a new terminal/command line window within an appropriate directory:
```
DATABASE_USERNAME=root DATABASE_NAME=train_database dtd2mysql --timetable /path/to/ttisxxx.ZIP
```
3. Run the following to download the GTFS files into the root directory:
```
DATABASE_USERNAME=root DATABASE_NAME=train_database dtd2mysql --gtfs-zip train_GTFS.zip
```
4. As [OpenTripPlanner (OTP)](http://www.opentripplanner.org/) requires GTFS files to not be stored in subfolders in the GTFS zip file, extract the downloaded 'train_GTFS.zip' and navigate to the subfolder level where the txt files are kept, then zip these files to a folder with an appropriate name (e.g., 'train_GTFS.zip'). 

**Note**: _if you are receiving a 'group_by' error, you will need to temporarily or permenantly disable `'ONLY_FULL_GROUP_BY'` in mysql._

### Cleaning the GTFS Data

It is unlikely that the converted GTFS ZIP files will work directly with [OpenTripPlanner (OTP)](http://www.opentripplanner.org/). Often this is caused by stops within the stop.txt file that are not handled by other parts of the GTFS feed, but there are other issues too. In propeR we have created a function called `cleanGTFS()` to clean and preprocess the GTFS files. To run:

```
#R
library(propeR)
cleanGTFS(gtfs.dir, gtfs.filename)
```

Where `gtfs.dir` is the directory where the GTFS ZIP folder is located, and `gtfs.filename` is the filename of the GTFS feed. This will create a new, cleaned GTFS ZIP folder in the same location as the old ZIP folder, but with the suffix '\_new'. Run this function for each GTFS feed.

### Sample GTFS data

The Data Science Campus as created some cleaned GTFS data for buses around [Cardiff, Wales](https://www.openstreetmap.org/#map=11/51.6700/-3.1600). This can be found on the propeR github page [here](https://github.com/datasciencecampus/access-to-services/tree/master/propeR/data/gtfs). This data was created using the steps above.

**Note**: _this GTFS may not contain the most recent timetables, it is only designed as a practice set of GTFS data for use with the propeR tool. Some (but not most) services have end dates of 2018-08-15, 2018-09-02, 2018-10-31. Therefore, anlysing journeys after these dates will not include these services. Most services have an end date capped at 2020-01-01._

## Creating the OpenTripPlanner server

[OpenTripPlanner (OTP)](http://www.opentripplanner.org/) is an open source multi-modal trip planner, which runs on Linux, Mac, Windows, or potentially any platform with a Java virtual machine. More details, including basic tutorials can be found [here](http://docs.opentripplanner.org/en/latest/). Guidance on how to setup the OpenTripPlanner locally can be found [here](https://github.com/opentripplanner/OpenTripPlanner/wiki). Here is the method that worked for us:

1) Check you have the latest java SE runtime installed on your computer, preferrably the 64-bit version on a 64-bit computer. The reason for this is that the graph building process in step 7 uses a lot of memory. The 32-bit version of java might not allow a sufficient heap size to be allocated to graph and server building. For the GTFS sample data [here](add link), a 32-bit machine may suffice. 
2) Create an 'otp' folder in a preferred root directory.
3) Download the latest single stand-alone runnable .jar file of OpenTripPlanner [here](https://repo1.maven.org/maven2/org/opentripplanner/otp/). Choose the '-shaded.jar' file. Place this in the 'otp' folder.
4) Create a 'graphs' folder in the 'otp' folder.
5) Create a 'default' folder in the 'graphs' folder.
6) Put the GTFS ZIP folder(s) in the 'default' folder along with the latest OpenStreetMap .osm data for your area, found [here](https://download.geofabrik.de/europe/great-britain/wales.html). If you're using the sample GTFS data, an .osm file for Cardiff can be found [here](https://github.com/datasciencecampus/access-to-services/tree/master/propeR/data/osm).
7) Build the graph by using the following command line/terminal command whilst in the 'otp' folder: 

```
java -Xmx4G -jar otp-1.3.0-shaded.jar --build graphs/default
```
  changing the shaded.jar file name and end folder name to be the appropriate names for your build. '-Xmx4G' specifies a maximum heap size of 4G memory, graph building may not work with less memory than this.
8) Once the graph has been build you should have a 'Graphs.obj' file in the 'graphs/default' folder. Now initiate the server using the following command from the 'otp' folder: 

```
java -Xmx4G -jar otp-1.3.0-shaded.jar --graphs graphs --router default --server
```
Again, checking the shaded.jar file and folder names are correct.
9) If successful, the front-end of OTP should be accessible from your browser using [http://localhost:8080/](http://localhost:8080/).

## Running propeR packages

The [README](https://github.com/datasciencecampus/propeR/blob/develop/README.md) will provide a guide on how to install propeR. As with any R package, it can be loaded in an R session using:

```
#R
library(propeR)
```

This will give you access to the the following functions:

| Function | Description |
|-----------------------|-----------------------------------------|
| choropleth | Calculates the journey time from multiple origins to a single destination, and produces a [choropleth map](https://en.wikipedia.org/wiki/Choropleth_map). |
| importLocationData | Used to generate a dataframe from a CSV file containing origin or destination information. |
| importGeojsonData | Used to create a polygon file from a GeoJSON file containing origin information. |
| postcodeToDecimalDegrees | Used in importLocationData() to convert postcodes to decimal degrees latitude and longitude via API calls (*needs internet access*). |
| cleanGTFS | Used to clean GTFS ZIP folder before OTP graph building. |
| isochrone | Generates a polygon [(isochrone)](https://en.wikipedia.org/wiki/Isochrone_map) around a single origin to calculate journey times to multiple destinations, can output a PNG map, HTML map, and .GeoJSON polygon file. |
| isochroneTime | Same as isochrone(), but between a start and end time/date. Output can be an animated GIF image. |
| isochroneMulti  | Same as isochrone(), but for multiple origins. A polygon is created for each origin. |
| isochroneMultiIntersect | Similar to isochroneMulti(), but generates a polygon for the intersection between multiple polygons from multiple origins. Each origin can have its own journey parameters. |
| isochroneMultiIntersectSensitivity | Same as isochroneMultiIntersect(), but applies a 30-minute window either side of start time. |
| isochroneMultiIntersectTime() | Same as isochroneMultiIntersect(), but between a start and end time/date. |
| otpChoropleth | A core function used to produce an API call to OTP to be used with choropleth(). |
| otpConnect | A core function used to connect to OTP either locally or remotely (i.e. the URL of the generated and hosted OTP graph). |
| otpIsochrone | A core function used to produce an API call to OTP to be used with the propeR isochrone functions. |
| otpTripDistance | A core function used to produce an API call to OTP to find trip distance. |
| otpTripTime | A core function used to produce an API call to OTP to find trip time. |
| pointToPoint | Calculates the journey details between a single origin and destination, can output a PNG map and HTML map. |
| pointToPointLoop | Calculates the journey details between multiple origins and destinations. |
| pointToPointTime | Same as pointToPoint(), but between a start and end time/date. Output can be an animated GIF image. |

### Data Prerequisites

All location data (origin and destination) must be in comma separated (CSV) format and contain the following columns:
* A unique ID column
* A latitude column, where data is in decimal degrees (or a postcode column)
* A longitude column, where data is in decimal degrees (or a postcode column)

The CSV file must contain headers, the header names can be specified in **`importLocationData()`**.

**Note**: _if using the **`choropleth()`** function the **name** column must match to the `origins` file._

### propeR Functions

Use **`?`** in R to view the function help files for more information, e.g., **`?isochrone`**. Below we will run through each function, but the help files will help you understand all the parameters that can be changed in each function.

#### otpConnect

The connection to the OTP server can be initiated using:

```
#R
otpcon <- otpConnect()
```

If you have named the folders differently to that suggested in [earlier in this guide](#creating-the-opentripplanner-server), you will need to specify these in the parameter fields (i.e. `hostname`, `router`, `port`). See **`?otpConnect`** for more information.

#### importLocationData and importGeojsonData

The propeR package comes with some sample CSV and GeoJSON data to be used alongside the OTP graph built using the sample GTFS and .osm files on the [github repo](https://github.com/datasciencecampus/propeR). To load this data, run:

```
#R
originPoints <- importLocationData(system.file("extdata", "origin.csv", package = "propeR"))
destinationPoints <- importLocationData(system.file("extdata", "destination.csv", package = "propeR"))
originPolygons <- importGeojsonData(system.file("extdata", "origin.geojson", package = "propeR"))
```

If using on non-sample data, replace `system.file("extdata", "origin.csv", package = "propeR")` with the full path of your data file. 

The sample data shows an example of data with a latitude, longitude column (recommended) in the origin CSV file, and one with a postcode column only (works, but not recommended) in the destination CSV file. The **`importLocationData()`** function will call a separate function (`postcodeToDecimalDegrees()`) that converts postcode to latitude and longitude. 

When loading your own data, you must specify the column name of the unique ID, latitude and longitude (or postcode) columns using the parameters `idcol`, `loncol`, `latcol` (or `postcodecol`). The defaults are name, lon, lat (and postcode), respectively.

The GeoJSON file is only required for the **`choropleth()`** function.

For the sample data importLocationData on the sample origin CSV file ('origin.csv') will load the following table:

|| objectid | lsoa11cd | name  | lat  | lon  | lat_lon |
|----|----------|----------|-------|------|------|---------|
| 5 | 33712 | W01001730 | Cardiff 008A | 51.52131 | -3.16326 | 51.52131,-3.16326  |
| 1 | 34128 | W01001888 | Cardiff 010A | 51.51275 | -3.23468 | 51.51275,-3.23468 | 
| 4 | 33303 | W01001755 | Cardiff | 025A | 51.50079 | -3.19173 | 51.50079,-3.19173 | 
| 2 | 33907 | W01001724 | Cardiff032C | 51.49211 | -3.17585 | 51.49211,-3.17585 | 
| 3 | 34039 | W01001715 | Cardiff 035B | 51.48688 | -3.21213 | 51.48688,-3.21213 | 

And the sample destination CSV file ('destination.csv') will load the following table:

|| name | postcode | lat  | lon  | lat_lon |
|----|------|----------|------|------|---------|
| 1 | Principality Stadium | CF10 1NS | 51.478284 | -3.182652 | 51.478284,-3.182652 | 
| 2 | Cardiff City Stadium | CF11 8AZ | 51.473246 | -3.211002 | 51.473246,-3.211002 | 


*Note:* _the column lat\_lon is generated automatically by `importLocationData()` and does not need to be manually entered._

#### pointToPoint

The most basic function in propeR is find the journey details for a trip with a single origin and destination. To do this, once the data has been loaded and the otp connection established, run the following:

```
#R
pointToPoint(output.dir = PATH_TO_DIR, 
              otpcon = otpcon, 
              originPoints = originPoints, 
              originPointsRow = 2, 
              destinationPoints = destinationPoints, 
              destinationPointsRow = 2, 
              startDateAndTime = '2018-08-18 12:00:00', 
              modes = 'WALK, TRANSIT',
              mapOutput = F)
```

For above - all other parameters left as default - the following journey details between Cardiff 010A (`originPointsRow = 2`) and Principality Stadium (`destinationPointsRow = 2`) are provided in a CSV file in the specified output directory:

| start    	| end      	| duration 	| walkTime | transitTime | waitingTime | transfers |
|----------	|----------	|----------	|---------- |------------- |------------- |-----------	|
| 12:11:10 	| 12:45:02 	| 33.87    	| 13.83    | 20          | 0.03        | 0         |

To output a PNG and interactive HTML leaflet map will as shown below, change the parameter `mapOutput` to `T`:

<p align="center"><img align="center" src="meta/images/pointToPoint.png" width="600px"></p>

Map colours, zoom and other parameters can be specified by the user. See ?pointToPoint for details.

#### pointToPointLoop

This function works in the similar way to the [`pointToPoint()`](#pointtopoint) function, but instead of a single origin and destination, the function loops through all origins and/or destinations provided.

```
#R
pointToPointLoop(output.dir = PATH_TO_DIR, 
              otpcon = otpcon, 
              originPoints = originPoints, 
              destinationPoints = destinationPoints, 
              journeyLoop = 0, 
              startDateAndTime = '2018-08-18 12:00:00', 
              modes = 'WALK, TRANSIT')
```

To loop just through the origins, set `journeyLoop` to `1`, to loop just through the destinations, set `journeyLoop` to `2`, and to loop through both, set `journeyLoop` to `0` (default). Also, you can calculate return leg journies by setting `journeyReturn` to `T` (default is `F`).

For the sample origin and destination locations, this following is produced:

| origin       | destination          | start_time | end_time | distance_km | duration_mins | walk\_time_mins | transit\_time_mins | waiting\_time_mins | transfers |
|--------------|----------------------|------------|----------|-------------|---------------|----------------|-------------------|-------------------|-----------|
| Cardiff 008A | Cardiff City Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 010A | Cardiff City Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 025A | Cardiff City Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 032C | Cardiff City Stadium | 12:07:58   | 12:49:16 | 4.91        | 41.3          | 17.27          | 12                | 12.03             | 1         |
| Cardiff 035B | Cardiff City Stadium | 12:13:01   | 12:54:16 | 4.86        | 41.25         | 14.25          | 8                 | 19                | 1         |
| Cardiff 008A | Principality Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 010A | Principality Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 025A | Principality Stadium | NA         | NA       | NA          | NA            | NA             | NA                | NA                | NA        |
| Cardiff 032C | Principality Stadium | 12:00:58   | 12:27:17 | 3           | 26.32         | 13.28          | 13                | 0.03              | 0         |
| Cardiff 035B | Principality Stadium | 12:13:01   | 12:55:17 | 6.06        | 42.27         | 10.27          | 13                | 19                | 1         |

#### pointToPointTime

This function works in the similar way to the [`pointToPoint()`](#pointtopoint) function, but instead of a single `startDateAndTime`, an `endDateAndTime` and `timeIncrease` (the incremental increase in time between `startDateAndTime` and `endDateAndTime` journeys should be analysed for) can be stated:

```
#R
pointToPointTime(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              originPointsRow = 2,
              destinationPoints = destinationPoints,
              destinationPointsRow = 2,
              startDateAndTime = '2018-08-18 12:00:00',
              endDateAndTime = '2018-08-18 13:00:00',
              timeIncrease = 20,
              modes = 'WALK, TRANSIT',
              mapOutput = F)
```

For example between Cardiff 010A and Principality Stadium - all other parameters left again as default - the following journey details are provided in a CSV file in the specified output directory:

| start    	| end      	| duration 	| walkTime 	| transitTime 	| waitingTime 	| transfers 	| date       	| date_time           	|
|----------	|----------	|----------	|----------	|-------------	|-------------	|-----------	|------------	|---------------------	|
| 12:11:36 	| 12:44:34 	| 32.97    	| 12.93    	| 20          	| 0.03        	| 0         	| 08/18/2018 	| 08/18/2018 12:11:36 	|
| 12:20:31 	| 12:54:19 	| 33.8    	| 17.77    	| 16          	| 0.03        	| 0         	| 08/18/2018 	| 08/18/2018 12:20:31 	|
| 12:51:36 	| 13:24:34 	| 32.97    	| 12.93    	| 20          	| 0.03        	| 0         	| 08/18/2018 	| 08/18/2018 12:51:36 	|
| 13:11:36 	| 13:44:34 	| 32.97    	| 12.93    	| 20          	| 0.03        	| 0         	| 08/18/2018 	| 08/18/2018 13:11:36 	|

Again, changing `mapOutput` to `T` will save a GIF map to the output directory:

<p align="center"><img align="center" src="meta/images/pointToPointTime.gif" width="600px"></p>

*Note:* _if left to the default mapZoom tries to set the zoom to the bounding box (`'bb'`) of the origin and destination locations, and the polyline created from the first API call; however, if the first call returns no journey, the map zoom level may not be appropriately set. If this is the case, you may need to manually enter an appropriate mapZoom number (e.g. `mapZoom = 12`)._

#### isochrone

Instead of a single origin (Cardiff 010A) to a single destination (Principality Stadium), the isochrone function works by taking a single origin (Cardiff 010A) and computing the maximum distance from this origin within specified cutoff times. This means that the travel time to multiple destinations (Principality Stadium and Cardiff City Stadium) can be analysed through a single OTP API call. For example for the default parameters:

```
#R
isochrone(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              originPointsRow = 2,
              destinationPoints = destinationPoints,
              startDateAndTime  = '2018-08-18 12:00:00',
              modes = 'WALK, TRANSIT',
              isochroneCutOffMax = 90,
              isochroneCutOffMin = 30,
              isochroneCutOffStep = 30,
              mapOutput = F,
              geojsonOutput = F)
```

Will output the message `The number of destinations that are within the maximum travel time is 1/2, or 50%` as only the Principality Stadium can be reached within the maximum specified cutoff time (90 minutes). To change the cutoffs use the function parameter `isochroneCutOffMax`, `isochroneCutOffMin` and `isochroneCutOffStep`. A tabular output will also show the travel time (in minutes) for each destination:

| name                 	| postcode 	| lat       	| lon       	| lat_lon             	| travel_time 	|
|----------------------	|----------	|-----------	|-----------	|---------------------	|-------------	|
| Cardiff City Stadium 	| CF11 8AZ 	| 51.473246   | -3.211002  	| 51.473246,-3.211002  	| NA         	|
| Principality Stadium 	| CF10 1NS 	| 51.478284 	| -3.182652 	| 51.478284,-3.182652 	| 60          	|

The NA result for the Cardiff City Stadium shows that is outside the maximum cutoff time. But the map (outputted by changing `mapOutput` to `T`) below shows that it only just outside (black circle):

<p align="center"><img align="center" src="meta/images/isochrone.png" width="600px"></p>

To save the polygon as a .GeoJSON file into the output folder, change `geojsonOutput` to `T`.

#### isochroneTime

This function is to [`isochrone()`](#isochrone) what [`pointToPointTime()`](#pointtopointtime) was to [`pointToPoint()`](#pointtopoint), i.e., a time-series between a start and end time/date at specified time intervals that produces a table and an optional animated GIF image. For the default parameters, running:

```
#R
isochroneTime(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              originPointsRow = 2,
              destinationPoints = destinationPoints,
              startDateAndTime = '2018-08-18 12:00:00',
              endDateAndTime = '2018-08-18 13:00:00',
              timeIncrease = 20,
              isochroneCutOffMax = 90,
              isochroneCutOffMin = 30,
              isochroneCutOffStep = 30,
              modes = 'WALK, TRANSIT',
              mapOutput = F)
```

The following table is produced:

| name                 	| postcode 	| lat       	| lon       	| lat_lon             	| 2018-08-18 12:00:00 	| 2018-08-18 12:20:00 	| 2018-08-18 12:40:00 	| 2018-08-18 13:00:00 	|
|----------------------	|----------	|-----------	|-----------	|---------------------	|---------------------	|---------------------	|---------------------	|---------------------	|
| Cardiff City Stadium 	| CF11 8AZ 	| 51.473246   | -3.211002   | 51.473246,-3.211002 	| NA                  	| NA                  	| 60                  	| NA                  	|
| Principality Stadium 	| CF10 1NS 	| 51.478284 	| -3.182652 	| 51.478284,-3.182652 	| 60                  	| 60                  	| 60                  	| 60                  	|

And the following GIF image if `mapOutput` is `T`:

<p align="center"><img align="center" src="meta/images/isochroneTime.gif" width="600px"></p>

*Note:* _if left to the default mapZoom tries to set the zoom to the bounding box (`'bb'`) of the origin and destination locations, and the polygon created from the first API call; however, if the first call returns no journey, the map zoom level may not be appropriately set. If this is the case, you may need to manually enter an appropriate mapZoom number (e.g. `mapZoom = 12`)._

#### 3.2.7. isochroneMulti

This function works similarly to the [`isochrone()`](#isochrone) function; however, it can handle multiple origins and multiple destinations. This is useful when considering the travel time between multiple locations to multiple possible destinations. For example, the sample data will show how travel time from two areas in Cardiff (Cardiff 010A and Cardiff 032C) to the two sports venues (Principality Stadium and Cardiff City Stadium). 

Using the default parameters, the function is run by:

```
#R
isochroneMulti(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              destinationPoints = destinationPoints,
              startDateAndTime  = '2018-08-18 12:00:00',
              modes = 'WALK, TRANSIT',
              isochroneCutOffMax = 90,
              isochroneCutOffMin = 30,
              isochroneCutOffStep = 30,
              mapOutput = F
              geojsonOutput = F)
```

Which produces the following table:

|              	|  Cardiff City Stadium	|  	Principality Stadium|
|--------------	|-----------------------|----------------------	|
| Cardiff 008A 	| 60                   	| 60                 	  |
| Cardiff 010A 	| NA                 	| 60                 	  |
| Cardiff 025A 	| 60                   	| 30                 	  |
| Cardiff 032C 	| 60                 	  | 30                 	  |
| Cardiff 035B 	| 60                 	  | 30                 	  |

Where the values are duration times in minutes. A PNG and interactive HTML map can also be saved in the output directory by changing `mapOutput` to `T`.

<p align="center"><img align="center" src="meta/images/isochroneMulti.png" width="600px"></p>

In addition, the polygons can be saved as a single .GeoJSON file by changing `geojsonOutput` to `T`.

#### isochroneMultiIntersect

This function finds the intersection of isochrone polygons from multiple origins. As a result, it can be slow on large datasets. Additional columns may be added to the origin CSV file to maximise this tool. They include:

* `mode` - the specific mode of transport that is usually specified within the function call (e.g., `'WALK, TRANSIT'` or `'CAR'`)
* `max_duration` - the maximum isochrone cutoff duration in minutes (e.g., `90`)
* `time` - the time of travel in 12 hour format (e.g., `'08:00am'`)
* `date` - the date of travel in YY-MM-DD format (e.g., `'2018-10-01'`)

Therefore, each origin may have its own `mode`, `max_duration`, `time` and `date`. If none are specified, the defaults or those called within the function (like normal) are used. We've included a new origin CSV file to show this feature. To load it, use:

```
#R
originPoints <- importLocationData(system.file("extdata", "origin2.csv", package = "propeR"))
```

Using the new origin CSv file, this function will output a map (below) and .GeoJSON file with the intersection polygon, both saved to the output directory.

```
#R
isochroneMultiIntersect(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              destinationPoints = destinationPoints)
```

<p align="center"><img align="center" src="meta/images/isochroneMultiIntersect.png" width="600px"></p>

#### isochroneMultiIntersectSensitivity

This function is similar to [`isochroneMultiIntersect()`](#isochronemultiintersect) but instead of a fixed time, as specified, it runs for a time window either side. The time window is specified in the function parameters as `timeSensitivity` (in minutes), and the time step is specified as `timeSensitivityStep` (in minutes). These default to `30` and `5` minutes, respectively. The function is run using:

```
#R
isochroneMultiIntersectSensitivity(output.dir = PATH_TO_DIR, 
              otpcon = otpcon,
              originPoints = originPoints,
              destinationPoints = destinationPoints,
              timeSensitivity = 30,
              timeSensitivityStep = 5)
```

Currently, the only output is an animated GIF file. This function is designed to show whether the `isochroneIntersect()` call is valid (little change in intersection of isochrones) or not.

<p align="center"><img align="center" src="meta/images/isochroneMultiIntersectSensitivity.gif" width="600px"></p>

#### isochroneMultiIntersectTime

Like [`isochroneMultiIntersectSensitivity()`](#isochronemultiintersectsensitivity) this is a visual function, which creates the isochrone intersections between a start and end date/time **only** for uniform parameters for each origin, i.e., the additional origin CSV columns in isochroneMultiIntersect are not considered. The output will be an animated GIF file in the output directory.

```
#R
isochroneMultiIntersectTime(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              destinationPoints = destinationPoints,
              startDateAndTime  = '2018-08-18 12:00:00',
              endDateAndTime  = '2018-08-18 13:00:00',
              timeIncrease = 20,
              modes = 'WALK, TRANSIT',
              isochroneCutOffs = 60)
```

<p align="center"><img align="center" src="meta/images/isochroneMultiIntersectTime.gif" width="600px"></p>

#### choropleth

This function uses the backbone of the `otpTripTime()` function to create a number of OTP API calls for multiple origins to a single destination. It then uses a .GeoJSON file to create polygons and colour them based on the journey details. As a result, the name of the origins between the CSV file and .GeoJSON file must match. Using the sample .GeoJSON file and origin CSV file, we can call this function using:

```
#R
choropleth(output.dir = PATH_TO_DIR,
              otpcon = otpcon,
              originPoints = originPoints,
              originPolygons = originPolygons,
              destinationPoints = destinationPoints,
              destinationPointsRow = 2,
              startDateAndTime = '2018-08-18 12:00:00',
              modes  = 'WALK, TRANSIT',
              durationCutoff = 60,
              waitingCutoff = 10,
              transferCutoff = 1)
```

This produces a series of choropleth maps (example below) and a table output.

| name         	| status 	| duration 	| waitingtime 	| transfers 	| duration_cat     	| waitingtime_cat  	| transfers_cat       	|
|--------------	|--------	|----------	|-------------	|-----------	|------------------	|------------------	|---------------------	|
| Cardiff 010A 	| OK     	| 32.97    	| 0.03        	| 0         	| Under 60 minutes 	| Under 10 minutes 	| Under 1 transfer(s) 	|
| Cardiff 032C 	| OK     	| 26.05    	| 0.03        	| 0         	| Under 60 minutes 	| Under 10 minutes 	| Under 1 transfer(s) 	|

As the table shows, specific duration, waiting time and transfer cutoffs can be specified within the function parameters using `durationCutoff`, `waitingCutoff` and `transferCutoff`. The output maps will be continuous (between a minimum and maximum value) and also discrete (above or below the thresholds).

<p align="center"><img align="center" src="meta/images/choropleth_duration.png" width="600px"></p>

<p align="center"><img align="center" src="meta/images/choropleth_duration_cat.png" width="600px"></p>

## FAQ

### Common Errors

Q: Why am I receiving the following error when running propeR?

```
Error in curl::curl_fetch_memory(url, handle = handle) : 
  Failed to connect to localhost port 8080: Connection refused
Called from: curl::curl_fetch_memory(url, handle = handle)
```

A: The OTP server has not been initiated. Please see [step 2.](#creating-the-opentripplanner-server) of this guide.

Q: Why am I receiving the following error when running propeR?

```
Error in paste0(otpcon, "/plan") : object 'otpcon' not found
```

A: The OTP connection has not been established. Please see [step 3.2.1.](#otpconnect) of this guide.

