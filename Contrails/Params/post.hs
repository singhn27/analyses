module Params.Post where

{- Parameters -}

month :: [(String, Int)]
month =  zip [ "January"
             , "February"
             , "March"
             , "April"
             , "May"
             , "June"
             , "July"
             , "August"
             , "September"
             , "October"
             , "November"
             , "December"
             ] [1..]

year :: [[Int]]
year = [replicate 12 x | x <- [1987..2013]]

opts :: [((String, Int), Int)]
opts = concat $  [zip month x | x <- year]
              ++ [zip (take 8 month) (replicate 8 2014)]

{- Functions -}

mkRequest :: ((String, Int), Int) -> String
mkRequest o = headstr ++ (sqlstrs ms ys) ++ varlist ++ (varspec ms ys) ++ vartype
   where ms = fst o
         ys = snd o

mkReqList :: IO [String]
mkReqList = return $ [mkRequest o | o <- opts]

{- Selectors and Constants -}

headstr ::    String
headstr =     "UserTableName=On_Time_Performance&DBShortName=On_Time&RawDataTable=T_ONTIME&"

sqlstrs ::    (String, Int) -> Int -> String
sqlstrs m y = "sqlstr=+SELECT+ORIGIN,DEST+FROM++T_ONTIME+WHERE+Month+=" ++
              (show (snd m)) ++ "+AND+YEAR=" ++ (show y)

varlist ::    String
varlist =     "&varlist=ORIGIN,DEST&grouplist=&suml=&sumRegion=&filter1=title=&filter2=titl\
              \e=&geo=All&time="

varspec ::    (String, Int) -> Int -> String
varspec m y = (fst m)   ++ "&timename=Month&GEOGRAPHY=All&XYEAR=" ++ 
              (show y)  ++ "&FREQUENCY=" ++ (show (snd m))

vartype ::    String
vartype =     "&VarName=YEAR&VarDesc=Year&VarType=Num&VarName=QUARTER&VarDesc=Quarter&VarTy\
              \pe=Num&VarName=MONTH&VarDesc=Month&VarType=Num&VarName=DAY_OF_MONTH&VarDesc=\
              \DayofMonth&VarType=Num&VarName=DAY_OF_WEEK&VarDesc=DayOfWeek&VarType=Num&Var\
              \Name=FL_DATE&VarDesc=FlightDate&VarType=Char&VarName=UNIQUE_CARRIER&VarDesc=\
              \UniqueCarrier&VarType=Char&VarName=AIRLINE_ID&VarDesc=AirlineID&VarType=Num&\
              \VarName=CARRIER&VarDesc=Carrier&VarType=Char&VarName=TAIL_NUM&VarDesc=TailNu\
              \m&VarType=Char&VarName=FL_NUM&VarDesc=FlightNum&VarType=Char&VarName=ORIGIN&\
              \VarDesc=Origin&VarType=Char&VarName=ORIGIN_CITY_NAME&VarDesc=OriginCityName&\
              \VarType=Char&VarName=ORIGIN_STATE_ABR&VarDesc=OriginState&VarType=Char&VarNa\
              \me=ORIGIN_STATE_FIPS&VarDesc=OriginStateFips&VarType=Char&VarName=ORIGIN_STA\
              \TE_NM&VarDesc=OriginStateName&VarType=Char&VarName=ORIGIN_WAC&VarDesc=Origin\
              \Wac&VarType=Num&VarName=DEST&VarDesc=Dest&VarType=Char&VarName=DEST_CITY_NAM\
              \E&VarDesc=DestCityName&VarType=Char&VarName=DEST_STATE_ABR&VarDesc=DestState\
              \&VarType=Char&VarName=DEST_STATE_FIPS&VarDesc=DestStateFips&VarType=Char&Var\
              \Name=DEST_STATE_NM&VarDesc=DestStateName&VarType=Char&VarName=DEST_WAC&VarDe\
              \sc=DestWac&VarType=Num&VarName=CRS_DEP_TIME&VarDesc=CRSDepTime&VarType=Char&\
              \VarName=DEP_TIME&VarDesc=DepTime&VarType=Char&VarName=DEP_DELAY&VarDesc=DepD\
              \elay&VarType=Num&VarName=DEP_DELAY_NEW&VarDesc=DepDelayMinutes&VarType=Num&V\
              \arName=DEP_DEL15&VarDesc=DepDel15&VarType=Num&VarName=DEP_DELAY_GROUP&VarDes\
              \c=DepartureDelayGroups&VarType=Num&VarName=DEP_TIME_BLK&VarDesc=DepTimeBlk&V\
              \arType=Char&VarName=TAXI_OUT&VarDesc=TaxiOut&VarType=Num&VarName=WHEELS_OFF&\
              \VarDesc=WheelsOff&VarType=Char&VarName=WHEELS_ON&VarDesc=WheelsOn&VarType=Ch\
              \ar&VarName=TAXI_IN&VarDesc=TaxiIn&VarType=Num&VarName=CRS_ARR_TIME&VarDesc=C\
              \RSArrTime&VarType=Char&VarName=ARR_TIME&VarDesc=ArrTime&VarType=Char&VarName\
              \=ARR_DELAY&VarDesc=ArrDelay&VarType=Num&VarName=ARR_DELAY_NEW&VarDesc=ArrDel\
              \ayMinutes&VarType=Num&VarName=ARR_DEL15&VarDesc=ArrDel15&VarType=Num&VarName\
              \=ARR_DELAY_GROUP&VarDesc=ArrivalDelayGroups&VarType=Num&VarName=ARR_TIME_BLK\
              \&VarDesc=ArrTimeBlk&VarType=Char&VarName=CANCELLED&VarDesc=Cancelled&VarType\
              \=Num&VarName=CANCELLATION_CODE&VarDesc=CancellationCode&VarType=Char&VarName\
              \=DIVERTED&VarDesc=Diverted&VarType=Num&VarName=CRS_ELAPSED_TIME&VarDesc=CRSE\
              \lapsedTime&VarType=Num&VarName=ACTUAL_ELAPSED_TIME&VarDesc=ActualElapsedTime\
              \&VarType=Num&VarName=AIR_TIME&VarDesc=AirTime&VarType=Num&VarName=FLIGHTS&Va\
              \rDesc=Flights&VarType=Num&VarName=DISTANCE&VarDesc=Distance&VarType=Num&VarN\
              \ame=DISTANCE_GROUP&VarDesc=DistanceGroup&VarType=Num&VarName=CARRIER_DELAY&V\
              \arDesc=CarrierDelay&VarType=Num&VarName=WEATHER_DELAY&VarDesc=WeatherDelay&V\
              \arType=Num&VarName=NAS_DELAY&VarDesc=NASDelay&VarType=Num&VarName=SECURITY_D\
              \ELAY&VarDesc=SecurityDelay&VarType=Num&VarName=LATE_AIRCRAFT_DELAY&VarDesc=L\
              \ateAircraftDelay&VarType=Num&VarName=FIRST_DEP_TIME&VarDesc=FirstDepTime&Var\
              \Type=Char&VarName=TOTAL_ADD_GTIME&VarDesc=TotalAddGTime&VarType=Num&VarName=\
              \LONGEST_ADD_GTIME&VarDesc=LongestAddGTime&VarType=Num&VarName=DIV_AIRPORT_LA\
              \NDINGS&VarDesc=DivAirportLandings&VarType=Num&VarName=DIV_REACHED_DEST&VarDe\
              \sc=DivReachedDest&VarType=Num&VarName=DIV_ACTUAL_ELAPSED_TIME&VarDesc=DivAct\
              \ualElapsedTime&VarType=Num&VarName=DIV_ARR_DELAY&VarDesc=DivArrDelay&VarType\
              \=Num&VarName=DIV_DISTANCE&VarDesc=DivDistance&VarType=Num&VarName=DIV1_AIRPO\
              \RT&VarDesc=Div1Airport&VarType=Char&VarName=DIV1_WHEELS_ON&VarDesc=Div1Wheel\
              \sOn&VarType=Char&VarName=DIV1_TOTAL_GTIME&VarDesc=Div1TotalGTime&VarType=Num\
              \&VarName=DIV1_LONGEST_GTIME&VarDesc=Div1LongestGTime&VarType=Num&VarName=DIV\
              \1_WHEELS_OFF&VarDesc=Div1WheelsOff&VarType=Char&VarName=DIV1_TAIL_NUM&VarDes\
              \c=Div1TailNum&VarType=Char&VarName=DIV2_AIRPORT&VarDesc=Div2Airport&VarType=\
              \Char&VarName=DIV2_WHEELS_ON&VarDesc=Div2WheelsOn&VarType=Char&VarName=DIV2_T\
              \OTAL_GTIME&VarDesc=Div2TotalGTime&VarType=Num&VarName=DIV2_LONGEST_GTIME&Var\
              \Desc=Div2LongestGTime&VarType=Num&VarName=DIV2_WHEELS_OFF&VarDesc=Div2Wheels\
              \Off&VarType=Char&VarName=DIV2_TAIL_NUM&VarDesc=Div2TailNum&VarType=Char&VarN\
              \ame=DIV3_AIRPORT&VarDesc=Div3Airport&VarType=Char&VarName=DIV3_WHEELS_ON&Var\
              \Desc=Div3WheelsOn&VarType=Char&VarName=DIV3_TOTAL_GTIME&VarDesc=Div3TotalGTi\
              \me&VarType=Num&VarName=DIV3_LONGEST_GTIME&VarDesc=Div3LongestGTime&VarType=N\
              \um&VarName=DIV3_WHEELS_OFF&VarDesc=Div3WheelsOff&VarType=Char&VarName=DIV3_T\
              \AIL_NUM&VarDesc=Div3TailNum&VarType=Char&VarName=DIV4_AIRPORT&VarDesc=Div4Ai\
              \rport&VarType=Char&VarName=DIV4_WHEELS_ON&VarDesc=Div4WheelsOn&VarType=Char&\
              \VarName=DIV4_TOTAL_GTIME&VarDesc=Div4TotalGTime&VarType=Num&VarName=DIV4_LON\
              \GEST_GTIME&VarDesc=Div4LongestGTime&VarType=Num&VarName=DIV4_WHEELS_OFF&VarD\
              \esc=Div4WheelsOff&VarType=Char&VarName=DIV4_TAIL_NUM&VarDesc=Div4TailNum&Var\
              \Type=Char&VarName=DIV5_AIRPORT&VarDesc=Div5Airport&VarType=Char&VarName=DIV5\
              \_WHEELS_ON&VarDesc=Div5WheelsOn&VarType=Char&VarName=DIV5_TOTAL_GTIME&VarDes\
              \c=Div5TotalGTime&VarType=Num&VarName=DIV5_LONGEST_GTIME&VarDesc=Div5LongestG\
              \Time&VarType=Num&VarName=DIV5_WHEELS_OFF&VarDesc=Div5WheelsOff&VarType=Char&\
              \VarName=DIV5_TAIL_NUM&VarDesc=Div5TailNum&VarType=Char"
