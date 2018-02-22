NRCan Housing Archetype Database
================================

Contents
--------
1) [Introduction](#intro)
1) [Objective](#objective) 
1) [Housing Archetype Classification](#classification) 

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

### Scope
This work predominately focuses energy use; factors affecting energy use (and therefore collected as part of the SHEU and Energuide databases) are collected. Others are not. 

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


[1]: http://www.nrcan.gc.ca/energy/efficiency/17097
