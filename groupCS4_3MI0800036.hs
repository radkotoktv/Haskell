----------------------------------------------------------------------------------------------------------------
-------------------------------------------------------A)-------------------------------------------------------
----------------------------------------------------------------------------------------------------------------

type Name = String

type Destination = String
type Price = Double
data Status = Upcoming
        | Canceled
        | InProgress
        | Finished
        deriving (Show, Eq)

type Duration = Double
type Activity = (Name, Duration)

type Trip = (Destination, Price, Status, [Activity])
type Points = Double

type Customer = (Name, [Trip], Points)

type Agency = [Customer]

data Thresholds = Thresholds { moneyThreshold :: Double, durationThreshold :: Double, loyaltyPointsThreshold :: Double }


----------------------------------------------------------------------------------------------------------------
----------------------------------------Printing functions (for testing)----------------------------------------
----------------------------------------------------------------------------------------------------------------


printActivities :: [Activity] -> IO()
printActivities [] = putStr ""
printActivities ((name, duration):xs)
    = do
        putStrLn "-------------------"
        putStrLn $ "Name: " ++ name
        putStr "Duration: "
        print duration
        printActivities xs

printTrips :: [Trip] -> IO()
printTrips [] = putStr ""
printTrips ((destination, price, status, activities):xs)
    = do
        putStrLn "-------------------"
        putStrLn $ "Destination: " ++ destination
        putStr "Price: "
        print price
        putStr "Status: "
        print status
        putStr "Activities: \n"
        printActivities activities
        printTrips xs

printCustomers :: [Customer] -> IO()
printCustomers [] = putStr ""
printCustomers ((name, trips, points):xs)
    = do
        putStrLn $ "Name: " ++ name
        putStrLn "-------------------"
        putStr "Trips\n"
        printTrips trips
        putStrLn "-------------------"
        putStr "Points: "
        print points
        putStrLn "-------------------"
        printCustomers xs

printAgency :: Agency -> IO()
printAgency [] = putStr ""
printAgency agency
    = do
        putStrLn "-------------------"
        printCustomers agency

----------------------------------------------------------------------------------------------------------------
--------------------------------------------------Example data--------------------------------------------------
----------------------------------------------------------------------------------------------------------------

trip1 :: Trip
trip1 = ("Paris", 50, Upcoming, [("Disney land", 30), ("Eiffel Tower", 10)])

trip2 :: Trip
trip2 = ("London", 40, Finished, [("London Eye", 5)])

trip3 :: Trip
trip3 = ("Madrid", 35, InProgress, [("Live Show", 15), ("Game show", 60)])

trip4 :: Trip
trip4 = ("Sofia", 10, Finished, [])

trip5 :: Trip
trip5 = ("Sofia", 5, InProgress, [("NDK", 2)])

trip6 :: Trip
trip6 = ("Tokyo", 200, Canceled, [("Sakura tree", 20), ("Japanese food", 40)])

trip7 :: Trip
trip7 = ("Tokyo", 200, Upcoming, [("Sakura tree", 20), ("Japanese food", 40)])

trip8 :: Trip
trip8 = ("Madrid", 35, Canceled, [("Live Show", 15), ("Game show", 60)])

customer1 :: Customer
customer1 = ("Ivan",[trip1, trip7], 40)

customer2 :: Customer
customer2 = ("Joro", [trip6, trip2], 0)

customer3 :: Customer
customer3 = ("Niki Kunchev", [trip2, trip3, trip8], 70)

customer4 :: Customer
customer4 = ("Ico", [trip4, trip5], 5)

customer5 :: Customer
customer5 = ("Maloumnik", [trip6, trip8], 100)

myAgency :: Agency
myAgency = [customer1, customer2, customer3, customer4, customer5]

----------------------------------------------------------------------------------------------------------------
------------------------------------------------Helper Functions------------------------------------------------
----------------------------------------------------------------------------------------------------------------

amountSpent :: Customer -> Double -- total money spent from a customer
amountSpent (_, [], _) = 0
amountSpent (name, (x:xs), points) = getPrice x + amountSpent (name, xs, points)

moneyEnough :: Customer -> Thresholds -> Bool
moneyEnough customer threshold = amountSpent customer >= moneyThreshold threshold

----------------------------------------------------------------------------------------------------------------

timeSpent :: Customer -> Double -- total time spent from a customer
timeSpent (_, [], _) = 0
timeSpent (name, ((_, _, _, []):other), points) = timeSpent (name, other, points)
timeSpent (name, ((destination, price, status, ((_, time):rest)):other), points) = time + timeSpent (name, (destination, price, status, rest):other, points)

timeEnough :: Customer -> Thresholds -> Bool
timeEnough customer threshold = timeSpent customer >= durationThreshold threshold

----------------------------------------------------------------------------------------------------------------

pointsSpent :: Customer -> Double -- the points of the customer
pointsSpent (_, [], _) = 0
pointsSpent (_, _, points) = points

pointsEnough :: Customer -> Thresholds -> Bool
pointsEnough customer threshold = pointsSpent customer >= loyaltyPointsThreshold threshold

----------------------------------------------------------------------------------------------------------------

hasCanceled :: Customer -> Bool -- if the customer has canceled a trip
hasCanceled (_, [], _) = False
hasCanceled (_, (_, _, Canceled, _):_, _) = True
hasCanceled (name, (_, _, Upcoming, _):other, points) = hasCanceled (name, other, points)
hasCanceled (name, (_, _, InProgress, _):other, points) = hasCanceled (name, other, points)
hasCanceled (name, (_, _, Finished, _):other, points) = hasCanceled (name, other, points)

----------------------------------------------------------------------------------------------------------------

allDestVisited :: Customer -> [Name]
allDestVisited (_, [], _) = []
allDestVisited (name, ((destination, _, _, _):other), points) = destination:allDestVisited (name, other, points)

hasDuplicateVisits :: Customer -> [Name] -> Bool -- if he has visited one location more than once
hasDuplicateVisits _ [] = False
hasDuplicateVisits customer (x:xs)
    | x `elem` xs = True
    | otherwise = hasDuplicateVisits customer xs

----------------------------------------------------------------------------------------------------------------
-----------------------------------------------Important Function-----------------------------------------------
----------------------------------------------------------------------------------------------------------------

currentThresholds :: Thresholds
currentThresholds = Thresholds 20 30 40

printName :: Customer -> Name
printName (name, _, _) = name

printNames :: [Customer] -> [Name]
printNames [] = []
printNames (x:xy) = (printName x):(printNames xy)

freeTripEligibleCustomers :: Agency -> Thresholds -> [Name]
freeTripEligibleCustomers agency threshold = printNames [x | x <- agency, moneyEnough x threshold, timeEnough x threshold, pointsEnough x threshold, hasCanceled x == False, hasDuplicateVisits x (allDestVisited x) == False]


----------------------------------------------------------------------------------------------------------------
-------------------------------------------------------B)-------------------------------------------------------
----------------------------------------------------------------------------------------------------------------


data Cancellation = Flexible
        | Moderate
        | Strict
        deriving Show

type CanceledTrip = (Trip, Cancellation)

----------------------------------------------------------------------------------------------------------------
------------------------------------------------Helper Functions------------------------------------------------
----------------------------------------------------------------------------------------------------------------

getPrice :: Trip -> Double
getPrice (_, price, _, _) = price

getAmountOfTrips :: Customer -> Double -- the amount of trips a customer has been on
getAmountOfTrips (_, [], _) = 0
getAmountOfTrips (name, (_:other), points) = 1 + getAmountOfTrips(name, other, points)

notCancelledTrips :: Customer -> Double -- the amount of trips the customer hasn't cancelled
notCancelledTrips (_, [], _) = 0
notCancelledTrips (name, x:xs, points)
    | getStatus x == Canceled = notCancelledTrips (name, xs, points)
    | otherwise = 1 + notCancelledTrips (name, xs, points)

sameTrip :: Trip -> Trip -> Bool -- if 2 trips are the same
sameTrip (_, _, _, []) (_, _, _, [])= True
sameTrip (_, _, _, []) (_, _, _, _) = False
sameTrip (_, _, _, _) (_, _, _, []) = False
sameTrip (destination, price, status, (activityName:rest)) (_destination, _price, _status, (_activityName:_rest))
        =   (destination == destination) && 
            (price == _price) && 
            (activityName == _activityName) && 
            sameTrip(destination, price, status, rest) (_destination, _price, _status, _rest)

getStatus :: Trip -> Status
getStatus (_, _, status, _) = status

cancels :: Customer -> [Trip] -- all the cancelled trips by the customer
cancels (_, [], _) = []
cancels (name, x:xs, points)
    | getStatus x == Canceled = x:cancels (name, xs, points)
    | otherwise = cancels (name, xs, points)

amountOfCancels :: Customer -> Double -- the amount of cancelled trips by the customer
amountOfCancels (_, [], _) = 0
amountOfCancels (name, x:xs, points)
    | getStatus x == Canceled = 1 + amountOfCancels (name, xs, points)
    | otherwise = amountOfCancels (name, xs, points)

canceledSum :: Customer -> Double
canceledSum (_, [], _) = 0
canceledSum (name, x:xs, points)
    | getStatus x == Canceled = (getPrice x) + canceledSum (name, xs, points)
    | otherwise = canceledSum (name, xs, points)

canceledAvg :: Customer -> Double
canceledAvg customer = (canceledSum customer)/(amountOfCancels customer)

canceledThisTrip :: Customer -> Trip -> Bool -- if the customer has canceled this specific trip
canceledThisTrip (_, [], _) _ = False
canceledThisTrip (name, x:xs, points) trip
    | sameTrip x trip && getStatus x == Canceled = True
    | otherwise = canceledThisTrip (name, xs, points) trip

amountOfPeopleCanceled :: Agency -> Trip -> Double -- the amount of people that have cancelled this specific trip
amountOfPeopleCanceled [] _ = 0
amountOfPeopleCanceled (x:xs) trip
    | canceledThisTrip x trip == True = 1 + amountOfPeopleCanceled xs trip
    | otherwise = amountOfPeopleCanceled xs trip

wentOnThisTrip :: Customer -> Trip -> Bool -- if the customer has gone on this specific trip
wentOnThisTrip (_, [], _) _ = False
wentOnThisTrip (name, x:xs, points) trip
    | sameTrip x trip = True
    | otherwise = wentOnThisTrip (name, xs, points) trip

amountOfPeopleOnTrip :: Agency -> Trip -> Double -- the amount of people that have gone on this specific trip
amountOfPeopleOnTrip [] _ = 0
amountOfPeopleOnTrip (x:xs) trip
    | wentOnThisTrip x trip = 1 + amountOfPeopleOnTrip xs trip
    | otherwise = amountOfPeopleOnTrip xs trip

quotient :: Agency -> Trip -> Double
quotient agency trip = (amountOfPeopleCanceled agency trip)/(amountOfPeopleOnTrip agency trip) 

getName :: Customer -> Name
getName (name, _, _) = name

getDestinations :: Customer -> [Destination]
getDestinations (_, [], _) = []
getDestinations (name, (destination, _, _, _):other, points) = destination:getDestinations(name, other, points)

getDestination :: Trip -> Destination
getDestination (destination, _, _, _) = destination

----------------------------------------------------------------------------------------------------------------
------------------------------------------------Useful function-------------------------------------------------
----------------------------------------------------------------------------------------------------------------

amountRefunded :: Agency -> Customer -> CanceledTrip -> Double -- the amount needed to be refunded
amountRefunded agency customer (trip, Flexible) = max ((getPrice trip) * 0.7 * ((notCancelledTrips customer)/(getAmountOfTrips customer)) - (quotient agency trip) * (canceledAvg customer)) 0
amountRefunded agency customer (trip, Moderate) = max ((getPrice trip) * 0.4 * ((notCancelledTrips customer)/(getAmountOfTrips customer)) - (quotient agency trip) * (canceledAvg customer)) 0
amountRefunded agency customer (trip, Strict) = max ((getPrice trip) * 0.1 * ((notCancelledTrips customer)/(getAmountOfTrips customer)) - (quotient agency trip) * (canceledAvg customer)) 0

--getRefund :: Agency -> Name -> Destination -> IO() -- has to call "amountRefunded" if it can be refunded
--getRefund (x:xs) customerName destination
--    = do
--       if getName x \= customerName then getRefund xs customerName destination
--        else if destination `elem` getDestinations x then 

    --Got mixed up in the CenceledTrip type :(