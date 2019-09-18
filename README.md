# Access to services

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

The purpose of this project is to develop a tool that uses open-source transport data to analyse multi-modal travel in the UK. The project was initiated after discussions with Welsh Government and pinning down the need for a tool that would offer travel information regarding access to health, social and other services. This information will help officials identify areas with access difficulties and help policy makers with up to date travel details. Welsh Government is planning to use this tool to address areas such as the [Welsh Index of Multiple Deprivation (WIMD)](https://gov.wales/statistics-and-research/welsh-index-multiple-deprivation/?lang=en), [Valleys Taskforce](https://gov.wales/topics/people-and-communities/communities/taskforce-for-the-valleys/?lang=en), [South Wales Metro](https://gov.wales/topics/transport/public/metro/?lang=en) and others.

## Research Questions
Examples of research questions that were discussed with the Welsh Government and can potential be answered using the tool are:


**Access to services:** What is the average return trip travel time to the nearest service, per Lower Super Output Area (LSOA)?
 * using public transport
 * using private transport
 * using any transport


**The Valleys Taskforce:** What is the proportion of people that can access a working place, per working age group per LSOA?
  * within 45 minutes
  * within a cost of £X
  * within X minutes of travel by any mode


**Cadw website:** What is the access to Cadw properties from tourist accommodations in specific travel times?
  * list of accommodations accessed by public transport
  * list of accommodations accessed by private transport
  * number of beds accessed by public transport
  * number of beds accessed by private transport


**South Wales Metro programme:** What are the benefits after the completion of Phase 2 of SWM programme in terms of:
  * total kilometres travelled by passengers using public transport
  * changes in the journey times, service intervals and transfers
  * rail and bus passengers
  * direct services between main residential areas and economic centres
  * traffic and congestion
  * access to employment opportunities


## Data

The project relies entirely on open-source data coming from different sources and in various formats. Data for car, bicycle, on-foot, bus and train travel is combined to build different scenarios and possible routes from multiple origins to multiple destinations, representing geographical areas (in Middle Layer Super Output Area (MSOA) or Lower Layer Super Output Area (LSOA) level) and services. The car, bicycle and foot travel information is provided from [OpenStreetMap](https://www.openstreetmap.org) that is built by a community of mappers who contribute and maintain data about roads, trails, cafés, railway stations, and much more, all over the world. The UK nationwide bus schedules and related data is provided in [TransXChange](https://www.gov.uk/government/collections/transxchange) format from the Department for Transport. The train data is provided in [Common Interface Format (CIF)](https://www.raildeliverygroup.com/our-services/rail-data/timetable-data.html) format from Network Rail, filtered to include only passenger train services.

The different nature of the data formats and the difficulty of combining them under a common tool, implied that we had to convert them in a more usable format. A common and very popular type of transport data is the [General Transit Feed Specification (GTFS)](https://en.wikipedia.org/wiki/General_Transit_Feed_Specification) as used by [Google](https://developers.google.com/transit/gtfs/reference/) to draw directions and calculate travel times in their maps. However, as far we know, in the UK only [Manchester](https://transitfeeds.com/p/transport-for-greater-manchester/224) and [London](https://tfl.gov.uk/info-for/open-data-users/) have open-source GTFS feeds. Therefore, our first task was to convert these formats to the more usable GTFS format.

Once all the data is converted, we can use [OpenTripPlanner](http://www.opentripplanner.org/), an open-source network builder, to built the multimodal transport network and run queries using the tool.

## graphite

To create a GTFS file, build and host an OTP server, please refer to [graphite](https://github.com/datasciencecampus/graphite).

## propeR

To analyse an OTP graph, you can use [propeR](https://github.com/datasciencecampus/proper).

## Authors / Contributors

#### Data Science Campus - Office for National Statistics
* [Michael Hodge](https://github.com/mshodge)
* [Phillip Stubbings](https://github.com/phil8192)
* [Jasmine Latham](https://github.com/jlathamONS)
* [Ioannis Tsalamanis](https://github.com/IoannisTsalamanis)

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details
