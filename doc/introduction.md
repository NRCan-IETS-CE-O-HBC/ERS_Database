NRCan Housing Archetype Database
================================

Contents
--------
1) [Introduction](#intro)
1) [Objective](#objective) 
1) [Scope](#scope) 
1) [Housing Archetype Classification](#classification) 
1) [Archetype database format](#databaseformat) 
1) [Methodology](#methods)

<a name="intro"></a>
Introduction 
------------
NRCan is developing a housing archetype database for examining energy use in Canada's residential housing stock. The housing archetype database is drawn from the following data sources:

__For new construction:__
- The [housing starts, under construction and completions][1] administered by Canadian Mortgage and Housing Corporation 
- The Energuide Housing Database, which contains over 6000 new EnergyStar house files

__For existing houses:__
- The [Survey of Household Energy Use (SHEU)][2] administered by Statistics Canada 
- The Energuide Housing Database, which contains pre- and post-retrofit audit data on 800,000 [check number and ref] Canadian homes
- Postal-code-data (to be written)

<a name="objective"></a>
Objective
---------
NRCan intends to develop a database that researchers, energy consultants, governments and utilities can use to examine:
+ typical characteristics of houses different regions, vintages and forms
+ how home characteristics vary within a region, vintage, form... 
+ how changes to a home's envelope, mechanical and renewable energy systems will impact home energy use and emissions

<a name="scope"></a>
Scope
---------
This work predominately focuses on the the physical attributes of a house that affect energy use. The scope of this work and the published data do not include:
- private information about a dwelling, including NRCan house ID, physical address or 6-digit postal code
- demographic information about home occupants
- behavior data about home occupants 
- climate data (although weather locations for housing archetypes will be reported)

Furthermore, the housing archetype database will not include energy consumption or emission estimates for the archetypes, although the archetype data will provide users with the means to compute such estimates. 

The scope is also limited to data currently collected as part of Energuide for housing database.

<a name="classification"></a>
Housing Archetype Classification 
--------------------------------
The archetypes will be classified according to the following topology:

__New houses:__
+ Housing market (`AT`,`QC`,`GTA`,`ON`,`PR`,`BC-LM`,`BC-int`,`TR`)
+ House type (`SD`,`Row-end unit`,`Row-mid unit`,`Semi-detached`,`MURB`)
+ Number of Storeys
+ Ceiling type (`Attic-hip`,`Attic-gable`,`Scissor`,`Cathedral`,`Flat`)
+ Foundation type(`Basement`,`Slab-on-grade`,`Crawl space`,`Walk-out`)
+ Window to wall ratio
+ Heated floor area

__Existing houses:__
+ Province / territory (`AB`,`BC`,`MB`,`NB`,`NF`,`NS`,`ON`,`PEI`,`QC`,`SK`,`TR`)
+ Vintage (`Before1946`,`1946-1960`,`1961-1977`,`1978-1983`,`1984-1995`,`1996-2000`,`2001-2005`,`2006-2010`,`2011-onwards`)
+ Space-heating fuel type & system (`Oil `,`Gas `,`Electric`,`Heat-pump`,`Other`,`Wood`,`Dual:wood/electric`,`Dual:wood/oil `,`Dual:gas/electric`,`Dual:oil/electric` )
+ Water heating fuel type & system (`Electric`,`Gas `,`Oil `,`Steam`,`Other `,`Wood`)
+ Housing form (`Single Detached`, `Single Attached`, `Apartment`,`Multi-Family`)
+ AC system (`AC-Room `,`AC-Central`, `AC-none`)
+ Rural/Urban distribution (Urban distributions may include major cities) 

<a name="database format"></a>
Archetype database format
-------------------------
NRCan will publish the database in 3 formats:
- Summary tables presenting average envelope and mechanical system characteristics for 
  each archetype, and weighting factors indicating how many homes that archetype represents across Canada
- Distributions containing measured average envelope and mechanical system characteristics from samples of 
  homes with similar archetype classifications, and weighting factors indicating how many homes 
- [HOT2000][3] files representing each of the sampled homes within the archetype classification.

### Format #1: Summary Tables
The summary tables will represent a single set of house characteristics for each housing archetype. Data will include mean and median measurements from all sampled homes matching the archetype characteristics. Measurements will include:
- Heated floor area
- Air tightness 
- Number of windows, Window U-value and SHGC
- Foundation type 
- Envelope insulation values (Ceilings, Walls, Foundations) 
- Heating system type, size and efficiency 
- Hot water system type, size and efficiency 
- Ventilation system flow rates and efficiencies 
- __Other characteristics ?__
  
NRCan anticipates that the summary tables will be most useful for understanding how housing characteristics vary by region, vintage and fuel type. Note that the housing characteristics provided in the summary table will not necessarily describe a single house, but rather a composite house made up from the average characteristics of all homes in the sample. Energy consumption estimates obtained from this data may approximate the average consumption of all homes in the sample, but the estimate may not be representative. 

The summary table will be published as a single comma-separated-value file, with each line describing the characteristics and mean measurements of a single archetype. 

### Format #2: Sample Distribution tables 
The sample distribution tables will have and identical format as the summary tables. But while the summary tables report a single data-point describing the mean characteristics of all sampled homes within the archetype classification, the sample distribution tables will report the measured data from the individual sampled homes. 

NRCan anticipates that the sample distribution table will be useful in understanding how stocks of homes vary within an archetype classification. Researchers and analysts may map the data from the sample distributions to building simulation software for the purposes of developing stock models, developing energy estimates, and scenario analysis (NRCan proposes to develop input files for one such stock model using [HTAP][4] and the [HOT2000][3] simulation platform - See [Format #3](#Format3).

The summary table will be published as a single comma-separated-value file, with each line describing the characteristics and measurements of a sampled house. 

<a name="Format3"></a>
### Format #3: HOT2000 files 
For each sample appearing in the sample distribution tables, NRCan will develop a representative [HOT2000][4] file (`.h2k`) for use in HOT2000 and [HTAP][4]. The mechanism for publishing these files. 
An automated script parse HOT2000 files to remove all private information, and rename files.


<a name="methods"></a>
Methods
-------------------------
To be developed. Prototype methods implemented as R-Script [ERS-DB-Analysis.r][5]

__Selection of new archetypes from the Energuide database:__
HTAP is designed to parse and modify xml files. Therefore, HOT2000v11[3] files can only be used in HTAP (previous versions of HOT2000[3] were using binary format). Energuide database consist houses from the EnergyStar[6] and R-2000[7] programs. HOT2000v10 is the delivery software for the R-2000 program. Therefore, only EnergyStar house files are available for archetype selection right now.
Available house files are examined to identify outliers and select an appropriate set which represents majority of new houses in each region. A ruby script randomly shuffles the remaining house files and select archetypes. A series of rules are applied to select representative houses in each market.
Process of archetypes selection is automated using ruby scripts. Therefore, archetypes can be updated in future using the same platform.

__Define weighting factors for each archetype:__
Participation in EnergyStar and R-2000 programs are not mandatory. Therefore, distribution of house types in the Energuide database is not representative of housing market in each region. It is important to define appropriate weighting factors for each archetype. These weighting factors are developed based on the statistical data in the CMHC[1] database. 
A user can extrapolate the results of energy consumption, GHG emissions, investment costs, and operating costs using weighting factors.

__Limitations:__
+ Limitations for selection of archetypes.

[1]: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3410012601
[2]: http://www.nrcan.gc.ca/energy/efficiency/17097
[3]: http://www.nrcan.gc.ca/energy/efficiency/housing/home-improvements/17725
[4]: https://github.com/NRCan-IETS-CE-O-HBC/HTAP
[5]: https://github.com/NRCan-IETS-CE-O-HBC/ERS_Database/blob/master/ERS-DB-Analysis.r
[6]: https://www.nrcan.gc.ca/energy/efficiency/housing/new-homes/5057
[7]: http://www.nrcan.gc.ca/energy/efficiency/homes/20575
