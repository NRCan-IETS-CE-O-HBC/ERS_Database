NRCan Housing Archetype Database
================================

Contents
--------
1) [Introduction](#intro)
1) [Objective](#objective) 
1) [Scope](#scope) 
1) [Housing Archetype Classification](#classification) 
1) [Archetype database format](#databaseformat) 

<a name="intro"></a>
Introduction 
------------
NRCan is developing a housing archetype database for examining energy use in Canada's residential housing stock. The housing archetype database is drawn from the follwing data sources:
- The [Survey of Household Energy Use (SHEU)][1] administered by Statistics Canada 
- The Energuide Housing Database, which contains pre- and post-retrofit audit data on 800,000 [check number and ref] Canadian homes
- Postal-code-data (to be written)

<a name="objective"></a>
Objective
---------
NRCan intends to develop a database that researchers, energy consultants, governments and utilities can use to examine:
+ typical characteristcs of houses different regions, vintages and forms
+ how home characteristics vary within a region, vintage, form... 
+ how changes to a home's envelope, mechanical and renewable energy systems will impact home energy use and emissions

<a name="scope"></a>
Scope
---------
This work predominately focuses on the the physical attributes of a house that affect energy use. The scope of this work and the published data do not not include:
- private information about a dwelling, including physical address or 6-digit postal code
- deomgraphic information about home occupants
- behavior data about home occupants 
- climate data (although weather locations for housing archetypes will be reported)

Furthermore, the housing archetype database will not include energy consumption or emission estimates for the archetypes, although the archetype data will provide users with the means to compute such estimates. 

The scope is also limited to data currently collected as part of Energuide for housing database.

<a name="classification"></a>
Housing Archetype Classification 
--------------------------------
The archetypes will be classified according to the following topology:
+ Province / territory (`AB`,`BC`,`MB`,`NB`,`NF`,`NS`,`ON`,`PEI`,`QC`,`SK`,`TR`)
+ Vintage (`Before1946`,`1946-1960`,`1961-1977`,`1978-1983`,`1984-1995`,`1996-2000`,`2001-2005`,`2006-2010`,`2011-onwards`)
+ Space-heating fuel type & system (`Oil `,`Gas `,`Electric`,`Heat-pump`,`Other`,`Wood`,`Dual:wood/electric`,`Dual:wood/oil `,`Dual:gas/electric`,`Dual:oil/electric` )
+ Water heating fuel type & system (`Electric`,`Gas `,`Oil `,`Steam`,`Other `,`Wood`)
+ Housing form (`Single Detached`, `Single Attached`, `Apartment`,`Multi-Family`)
+ AC system (`AC-Room `,`AC-Central`, `AC-none`)
+ Rural/Urban distribution (Urban distribtuions may include major cities) 

<a name="databaseformat"></a>
Archetype database format
-------------------------
NRCan will publish the database in 3 formats:
- Summary tables presenting average envelope and mechanical system characteristics for 
  each archetype, and weighting factors indicating how many homes that archetype represents across Canada
- Distributions containing measured average envelope and mechanical system characteristics from samples of 
  homes with similar archetype classifications, and weighting factors indicating how many homes 
- [HOT2000][2] files representing each of the sampled homes within the archetype classification.

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
  `Other characteristics ...`
  
NRCan anticipates that the summary tables will be most useful for understanding how housing characteristics vary by region, vintage adn fuel type. Note that the housing characteristics provided in the summary table will not necessarly describe. 





[1]: http://www.nrcan.gc.ca/energy/efficiency/17097
[2]: http://www.nrcan.gc.ca/energy/efficiency/housing/home-improvements/17725
