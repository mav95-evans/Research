
mi svyset [pweight=weight]
clear all
import delimited C:\Users\mav95\Documents\AHSFINALDATASET.csv
replace rentsub_new = . if rentsub_new == -1
replace fs = . if fs == -1
replace rentcntrl = . if rentcntrl == -1
replace nhqschool = . if nhqschool == -1
replace nhqpcrime = . if nhqpcrime == -1
replace nhqscrime = . if nhqscrime == -1
replace nearbarcl = . if nearbarcl == -1
replace nearaband = . if nearaband == -1
replace unit_size_new = . if unit_size_new == -1
replace ratinghs = . if ratinghs == -9
replace ratingnh = . if ratingnh == -9

mi set mlong

mi register imputed ratinghs ratingnh rentsub_new fs rentcntrl nhqschool nhqpcrime nhqscrime nearbarcl nearaband unit_size_new

mi impute mvn ratinghs ratingnh unit_size_new rentsub_new fs nhqschool nhqscrime nearaband = rent i.omb13cbsa hhsex_new hhraceall hhmar_new hhimmigrant hhspan_new hhage hincp utilamt i.hshldtype i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms, add(5) noisily  rseed(42)

mi impute chained (ologit) unit_size_new (logit) fs nhqschool nhqscrime nearbarcl = rent i.omb13cbsa hhsex_new i.hhraceall hhimmigrant hhspan_new hincp, add(5) noisily  rseed(42) 

mi svyset estimate regress rent hhsex_new##hhraceall i.omb13cbsa hhmar_new hhimmigrant hhspan_new hhage hincp utilamt i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms unit_size_new rentsub_new fs nhqscrime nearaband
 

logit costburden hhsex_new##hhraceall i.omb13cbsa hhimmigrant hhspan_new hhage hincp i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms i.unit_size_new i.fs i.rentcntrl i.nhqpcrime i.nearbarcl [pweight=weight]

mi estimate: regress rent hhsex_new##hhraceall i.omb13cbsa hhimmigrant hhspan_new hhage hincp i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms i.unit_size_new i.fs i.rentsub_new i.nhqpcrime i.nearbarcl [pweight=weight]

*use this
mi impute chained (ologit) unit_size_new (logit) fs nhqpcrime nearbarcl rentsub_new = rent crowd hhimmigrant i.omb13cbsa i.upkeep hhspan_new hhage hincp costburden hhsex_new i.adequacy hincp i.hhraceall, add(5) rseed(42)

*cost burden
mi estimate: logit costburden i.omb13cbsa hhsex_new##hhraceall hhsex_new##hhspan_new hhmar_new i.hhgrad utilamt hhimmigrant hhage hincp i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms i.unit_size_new i.fs i.rentsub_new i.nhqpcrime i.nearbarcl [pweight=weight]

*rent
mi estimate: regress rent hhsex_new##hhraceall hhsex_new##hhspan_new i.omb13cbsa hhsex_new##hhimmigrant hhmar_new i.hhgrad hhage hincp utilamt i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy numpeople totrooms i.unit_size_new i.fs i.rentsub_new i.nhqpcrime i.nearbarcl [pweight=weight]

*crowd
mi estimate: regress crowd rent hhsex_new##hhraceall hhsex_new##hhspan_new i.omb13cbsa hhimmigrant hhmar_new hhage hincp utilamt i.hhgrad i.hshldtype i.hudsub i.bld i.lotsize i.upkeep i.adequacy i.unit_size_new i.fs i.rentsub_new i.nhqpcrime i.nearbarcl [pweight=weight]
