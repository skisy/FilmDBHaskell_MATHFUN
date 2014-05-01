-- 
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2013/14 
-- Epic Movie database 9001
-- Student ID: 660273
--

import Data.List        -- imported for list functions (map, foldr, etc..)
import Data.Char        -- imported to check "numerical" inputs
import Text.Printf      -- imported to format ratings (of type float)

--
-- Types
--
type Title      = String
type Director   = String
type Year       = Int
type User       = String
type Rating     = (User, Float)

-- Defining algebraic type of Film
data Film       = Film Title Director Year [Rating]
                  deriving(Eq,Show,Read)

-- Declaration of instance of Ord for type Film based on average ratings of films
instance Ord Film where
    compare film1 film2
        | averageRating film1 > averageRating film2 = LT
        | averageRating film1 < averageRating film2 = GT
        | otherwise                                 = EQ

-- Test data for testing functions without persistence        
testDatabase :: [Film]
testDatabase = [Film "Blade Runner" "Ridley Scott" 1982 [("Amy",6), ("Bill",9), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",5), ("Megan",4)],
                Film "The Fly" "David Cronenberg" 1986 [("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",5)],
                Film "Psycho" "Alfred Hitchcock" 1960 [("Bill",4), ("Jo",4), ("Garry",8), ("Kevin",7), ("Olga",8), ("Liz",10), ("Ian",9)],
                Film "Body Of Lies" "Ridley Scott" 2008 [("Sam",3), ("Neal",7), ("Kevin",2), ("Chris",5), ("Olga",6)],
                Film "Avatar" "James Cameron" 2009 [("Olga",2), ("Wally",8), ("Megan",9), ("Tim",5), ("Zoe",8), ("Emma",3)],
                Film "Titanic" "James Cameron" 1997 [("Zoe",7), ("Amy",2), ("Emma",5), ("Heidi",3), ("Jo",8), ("Megan",5), ("Olga",7), ("Tim",10)],
                Film "The Departed" "Martin Scorsese" 2006 [("Heidi",2), ("Jo",8), ("Megan",5), ("Tim",2), ("Fred",5)],
                Film "Aliens" "Ridley Scott" 1986 [("Fred",8), ("Dave",6), ("Amy",10), ("Bill",7), ("Wally",2), ("Zoe",5)],
                Film "Prometheus" "Ridley Scott" 2012 [("Garry",3), ("Chris",4), ("Emma",5), ("Bill",1), ("Dave",3)],
                Film "E.T. The Extra-Terrestrial" "Steven Spielberg" 1982 [("Ian",7), ("Amy",2), ("Emma",7), ("Sam",8), ("Wally",5), ("Zoe",6)],
                Film "The Birds" "Alfred Hitchcock" 1963 [("Garry",7), ("Kevin",9), ("Olga",4), ("Tim",7), ("Wally",3)],
                Film "Goodfellas" "Martin Scorsese" 1990 [("Emma",7), ("Sam",9), ("Wally",5), ("Dave",3)],
                Film "The Shawshank Redemption" "Frank Darabont" 1994 [("Jo",8), ("Sam",10), ("Zoe",3), ("Dave",7), ("Emma",3), ("Garry",10), ("Kevin",7)],
                Film "Gladiator" "Ridley Scott" 2000 [("Garry",7), ("Ian",4), ("Neal",6), ("Wally",3), ("Emma",4)],
                Film "The Green Mile" "Frank Darabont" 1999 [("Sam",3), ("Zoe",4), ("Dave",8), ("Wally",5), ("Jo",5)],
                Film "True Lies" "James Cameron" 1994 [("Dave",3), ("Kevin",4), ("Jo",0)],
                Film "Minority Report" "Steven Spielberg" 2002 [("Dave",5), ("Garry",6), ("Megan",2), ("Sam",7), ("Wally",8)],
                Film "The Wolf of Wall Street" "Martin Scorsese" 2013 [("Dave",6), ("Garry",6), ("Megan",0), ("Sam",4)],
                Film "War Horse" "Steven Spielberg" 2011 [("Dave",6), ("Garry",6), ("Megan",3), ("Sam",7), ("Wally",8), ("Zoe",8)],
                Film "Lincoln" "Steven Spielberg" 2012 [("Ian",3), ("Sam",7), ("Wally",3), ("Zoe",4), ("Liz",7), ("Megan",4)],
                Film "Vertigo" "Alfred Hitchcock" 1958 [("Bill",7), ("Emma",5), ("Zoe",9), ("Olga",6), ("Tim",10)],
                Film "The Terminal" "Steven Spielberg" 2004 [("Olga",3), ("Heidi",8), ("Bill",2), ("Sam",6), ("Garry",8)],
                Film "Jaws" "Steven Spielberg" 1975 [("Fred",3), ("Garry",0), ("Jo",3), ("Neal",9), ("Emma",7)],
                Film "Hugo" "Martin Scorsese" 2011 [("Sam",4), ("Wally",3), ("Zoe",4), ("Liz",7)]]

----------------------------
--
-- FUNCTIONAL CODE
--
----------------------------

--
-- Library Functions
--

-- Checks if title matches title of film already existing in database
-- Returns True if match found or False if no match found
filmExists :: [Film] -> Title -> Bool
filmExists filmDB title = if ((filter (\(Film t _ _ _) -> t == title) filmDB) == []) then False else True

-- Validates string ([Char]) is comprised of only numbers (i.e. characters 0 - 9)
-- Passes head of string to isDigit (function in Data.Char) which verifies it is a numerical digit
-- Recursive call with tail of string to test next character
-- If isDigit returns True for all (&&) then function returns True otherwise False
isNumerical :: [Char] -> Bool
isNumerical []      = True
isNumerical (x:xs)  = isDigit x && isNumerical xs

-- Validates string is a valid full year (e.g. 2014 not just 14, '14, etc...)
-- Ensures length is 4 and all characters are numerical
isValidYear :: [Char] -> Bool
isValidYear year 
	| length year /= 4			= False
	| not (isNumerical year)	= False
	| otherwise					= True

-- Checks string matches a valid rating (does not check range because of need for type conversion first which can cause errors)
-- Returns True if rating consists of only numerical characters when decimal point stripped and decimal point is not first or last character
isValidRating :: [Char] -> Bool
isValidRating rating
    | not (isNumerical (rating \\ "."))                 = False
    | ((head rating) == '.' || (last rating) == '.')    = False
    | otherwise                                         = True

-- Formats Film type into nicely formatted string including average rating
showFilm (Film title director year ratings) = "\n Title: " ++ title ++ "\n Director: " ++ director ++ "\n Year: " ++ show year ++ "\n Rating: " ++ (if (isNaN (read rating :: Float)) then "0" else rating) ++ "\n"
    where rating = printf "%3.1f" (averageRating (Film title director year ratings))

-- Calculates average of a list
average xs = sum xs / genericLength xs

-- Takes Film as input and passes list of the numbers from the film ratings
-- Returns average rating
averageRating :: Film -> Float
averageRating (Film _ _ _ ratings) = (if (isNaN result) then 0 else result)
    where result = average (map snd ratings)

-- i. ADD NEW FILM
-- ==================
-- Adds to new Film to end of the list of Films
addFilm :: [Film] -> Title -> Director -> Year -> [Film]
addFilm filmDB title director year = filmDB ++ [Film title director year []]

-- ii. GIVE ALL FILMS
-- ==================
-- Takes list of films
-- Maps list of films to showFilm creating a list of strings
-- Uses foldr to reduce list of strings to single string
-- Returns result (all films in single string)
-- create a string with all films 
showFilms :: [Film] -> String
showFilms [] = "\n No films found!"
showFilms filmDB = foldr (++) "" (map showFilm filmDB)


-- iii. GIVE ALL FILMS BY DIRECTOR
-- ===============================
-- Uses filter to return list of films where the director matches director argument
showDirectorFilms :: [Film] -> Director -> [Film]
showDirectorFilms filmDB director = filter (\(Film _ d _ _) -> d == director) filmDB

-- iv. GIVE ALL FILMS WITH RATING >= 6
-- ===================================
-- Takes list of films
-- Returns list of films which have an average rating of 6 or over (from original list)
showSixRatedFilms :: [Film] -> [Film]
showSixRatedFilms filmDB = [ film | film <- filmDB, averageRating film >= 6 ]

-- v. GIVE AVERAGE RATING FOR FILMS BY DIRECTOR
-- =============================================
-- Calculates average rating for a director based on the ratings of all their films in the provided list of films
-- Uses showDirectorFilms to get list of films by director and maps to averageRating to get average for each film
-- Calculates and formats (to 1 decimal place) average of list of average ratings
-- Returns average as float
showDirectorAverage :: [Film] -> Director -> Float
showDirectorAverage filmDB director = read (printf "%3.1f" (average (map averageRating (showDirectorFilms filmDB director)))) :: Float

-- vi. GIVE TITLES RATED BY USER WITH USERS RATINGS
-- ================================================
-- Returns a list of tuples containing film titles and specified user's ratings
-- Uses list comprehension to return tuple of title and rating where rating is only the rating given by the specified user
-- Uses ++ and recursive call to add another (title, rating) tuple to the list
getUserRatings :: [Film] -> User -> [(Title, Float)]
getUserRatings (Film title _ _ ratings:[]) user       = [ (title, rating) | (u, rating) <- ratings, user == u ]
getUserRatings (Film title _ _ ratings:films) user    = [ (title, rating) | (u, rating) <- ratings, user == u ] ++ getUserRatings films user

-- Creates list of strings based on list of (title, rating) tuples returned by getUserRatings
-- Returns single formatted string containing all ratings of specified user (including film title for each eating) by
-- using unlines (equivalent to foldr (++) "") to join the strings in the list
showUserRatings :: [Film] -> User -> String
showUserRatings filmDB user     = unlines ["\n Title: " ++ title ++ "\n Rating: " ++ printf "%3.1f" rating | (title, rating) <- (getUserRatings filmDB user)]

-- vii. ALLOW USER TO RATE/RE-RATE FILM
-- ====================================
-- Finds specified film using filter and extracts from list using head then passes to passRating function with provided rating
-- Adds result of passRating (Film with new/updated rating) to rest of film database
-- Returns new list of films with updated specified user rating on specified film
rateFilm :: [Film] -> Title -> Rating -> [Film]
rateFilm filmDB title rating   = (filter (\(Film t _ _ _) -> t /= title) filmDB) ++ [(passRating (head (filter (\(Film t _ _ _) -> t == title) filmDB)) rating)]

-- Returns film passed with updated rating
passRating :: Film -> Rating -> Film
passRating (Film t d y ratings) rating  = (Film t d y (addRating ratings rating))

-- Filters out rating by specified user (if exists) and adds new Rating using specified user and rating value
-- Returns new list of Ratings
addRating :: [Rating] -> Rating -> [Rating]
addRating oldRatings (user, rating) = (filter(\(u, r) -> u /= user) oldRatings) ++ [(user, rating)]

-- viii. GIVE ALL FILMS RELEASED DURING OR AFTER YEAR, SORTED IN DESC ORDER OF RATING
-- ==================================================================================
-- Filter used to obtain list of films with year greater than or equal to specified year
-- Filtered list is then sorted using instance of Ord for type Film as declared previously
-- Returns list of Films ordered by average rating
showRatingOrderedYearFilms :: [Film] -> Year -> [Film]
showRatingOrderedYearFilms filmDB year = sort (filter (\(Film _ _ y ratings) -> y >= year) filmDB)


-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).
demo :: Int -> IO ()
demo 1  = putStrLn (showFilms (addFilm testDatabase "Gravity" "Alfonso Cuaron" 2013))
demo 2  = putStrLn (showFilms testDatabase)
demo 3  = putStrLn (showFilms (showDirectorFilms testDatabase "James Cameron"))
demo 4  = putStrLn (showFilms (showSixRatedFilms testDatabase))
demo 5  = putStrLn (show (showDirectorAverage testDatabase "James Cameron"))
demo 6  = putStrLn (showUserRatings testDatabase "Zoe")
demo 7  = putStrLn (showFilms (rateFilm testDatabase "Jaws" ("Zoe", 8)))
demo 77 = putStrLn (showFilms (rateFilm testDatabase "Vertigo" ("Zoe", 3)))
demo 8  = putStrLn (showFilms (showRatingOrderedYearFilms testDatabase 2009))

----------------------------
--
-- IO Code
--
----------------------------

-- Entry point for interface
-- Passes loaded database (list of films) to function to get username
main :: IO ()
main = do 
    filmDB <- loadDB
    putStrLn "-------------------------------------------------------------------------------\n"
    putStrLn "              Welcome to the FUN Haskell Film Rating Database"
    putStrLn "                             Author: 660273"
    putStrLn "\n-------------------------------------------------------------------------------\n"
    getUserName filmDB

-- Loads film database from file
-- Returns list of films (from file)
loadDB :: IO [Film]
loadDB = do 
    filmDB <- readFile "films.txt"
    filmDB `seq` return (read filmDB :: [Film])  -- Will always return (2nd argument) the file as list of films
    -- Without previous line, writeFile encounters errors due to resource being busy as a result of Haskell Lazy IO
 
-- Gets user's name from user input
-- Assuming the name is like a 'username' for a website where various numbers and other characters can be allowed and therefore input is not restricted to just letters but must not be null or just spaces
-- Calls menu function with user name and film database
getUserName :: [Film] -> IO ()
getUserName filmDB = do
    putStrLn "-------------------------------------------------------------------------------\n"
    putStr " Enter User Name: "
    user <- getLine
    putStrLn "\n-------------------------------------------------------------------------------\n"
    if (lex user) /= [("","")]
        then selectMenu filmDB user
        else do
            putStrLn " Please enter a valid user name" 
            putStrLn ""
            getUserName filmDB

-- Displays a textual menu and passes user input to processOption to determine action            
selectMenu :: [Film] -> User -> IO ()
selectMenu filmDB user = do
    putStrLn "\n===============================================================================\n"
    putStrLn "                                    Menu\n"
    putStrLn "===============================================================================\n"
    putStrLn " Please choose from the options below:\n"
    putStrLn " 1 - Add new film"
    putStrLn " 2 - Show all films"
    putStrLn " 3 - Show all films by a specific director"
    putStrLn " 4 - Show all films with an average rating of 6 or higher"
    putStrLn " 5 - Show the overall average rating of all films by a specific director"
    putStrLn " 6 - Show your film ratings"
    putStrLn " 7 - Rate a film"
    putStrLn " 8 - Show all films released on and after a specific year"
    putStrLn " 9 - Exit and discard changes"
    putStrLn " 0 - Exit and save changes"
    putStrLn "\n-------------------------------------------------------------------------------\n"
    putStr " Option: "
    option <- getLine
    putStrLn "\n-------------------------------------------------------------------------------\n"
    processOption option filmDB user

-- Pauses program temporarily to for better readability of returned results
continue :: [Film] -> User -> IO ()
continue filmDB user  = do
    putStr "\n Press Enter to continue \n\n"
    action <- getLine
    selectMenu filmDB user

-- Function was going to check if user had input 'menu' at any point and then check result of function 
-- to return to menu or continue with current function, but decided not to implement due to potential
-- for inappropriate action i.e. if user enters 'menu' for title, director, user (you never know)
--returnToMenu :: String -> Bool
--returnToMenu "menu"	= True
--returnToMenu  _		= False

-- Determines action to be taken based on passed user input
processOption :: String -> [Film] -> User -> IO ()
processOption "1" filmDB user   = do
	putStrLn "===============================================================================\n"
	putStrLn "                               Add a New Film\n"
	putStrLn "===============================================================================\n"
	newFilmDB <- addFilmIO filmDB
	continue newFilmDB user
processOption "2" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                                 All Films\n"
    putStrLn "===============================================================================\n"
    putStrLn (showFilms filmDB)
    continue filmDB user
processOption "3" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                          Show Films By Director\n"
    putStrLn "===============================================================================\n"
    putStr " Director: "
    director <- getLine
    putStrLn (showFilms (showDirectorFilms filmDB director))
    continue filmDB user
processOption "4" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                   Show Films With Rating of 6 or Higher\n"
    putStrLn "===============================================================================\n"
    putStrLn (showFilms (showSixRatedFilms filmDB))
    continue filmDB user
processOption "5" filmDB user   = do
	putStrLn "===============================================================================\n"
	putStrLn "                    Show Average Rating of a Director\n"
	putStrLn "===============================================================================\n"
	average <- averageDirectorRatingIO filmDB
	if (average == 0)
		then do
			putStrLn "\n           This director's films have not yet been rated!" 
			continue filmDB user
		else do
			putStr "     Average Rating: "
			putStr (show average)
			putStr "\n"
			continue filmDB user
processOption "6" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                            Show Your Ratings\n"
    putStrLn "===============================================================================\n"
    let ratings = showUserRatings filmDB user
    if (ratings == "") then putStrLn " You have not rated any films!" else putStrLn ratings
    continue filmDB user
processOption "7" filmDB user   = do
	putStrLn "===============================================================================\n"
	putStrLn "                               Rate a Film\n"
	putStrLn "===============================================================================\n"
	newFilmDB <- rateFilmIO filmDB user
	continue newFilmDB user
processOption "8" filmDB user   = do
	putStrLn "===============================================================================\n"
	putStrLn "               Show Films Release On and After Specific Year\n"
	putStrLn "===============================================================================\n"
	showYearFilmsIO filmDB
	continue filmDB user
processOption "9" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                         Exit and Discard Changes?\n"
    putStrLn "===============================================================================\n"
    putStr " Type 'y' to quit or anything else to return to the menu: "
    choice <- getLine
    if (choice == "y") then putStrLn "\n Goodbye!\n" else selectMenu filmDB user
processOption "0" filmDB user   = do
    putStrLn "===============================================================================\n"
    putStrLn "                          Exit and Save Changes?\n"
    putStrLn "===============================================================================\n"
    putStr " Type 'y' to quit or anything else to return to the menu: "
    choice <- getLine
    if (choice == "y") then saveAndQuit filmDB else selectMenu filmDB user
processOption _ filmDB user		= do
	putStrLn "===============================================================================\n"
	putStrLn "                 Invalid Input - Please choose a valid action\n"
	putStrLn "   Hint: Type the corresponding number of the desired action from the menu\n"
	putStrLn "===============================================================================\n"
	continue filmDB user

-- Gets film details from user input and checks their validity (film doesn't already exist and 
-- year is a number 4 digits long)
-- Returns new database with film added
addFilmIO :: [Film] -> IO [Film]
addFilmIO filmDB	= do
	putStr "                   Film Title: "
	title <- getLine
	putStr "\n                Film Director: "
	director <- getLine
	putStr "\n            Film Release Year: "
	year <- getLine
	if (filmExists filmDB title)
		then do
			putStrLn "\n Film already exists!"
			return filmDB
        else do
            if (isValidYear year)
            then do
                let newFilmDB = addFilm filmDB title director (read year :: Int)
                putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n" 
                putStrLn "                                 Film added!"
                putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
                return newFilmDB
                else do
                    putStrLn "\n You must enter a valid full year as a number (e.g. 2013)\n"
                    addFilmIO filmDB

-- Takes name of director as input from user and returns average ratings of 
-- directors films or 0 if films are not rated or don't exist (if NaN)                    
averageDirectorRatingIO :: [Film] -> IO Float
averageDirectorRatingIO filmDB	= do
	putStr "           Director: "
	director <- getLine
	let average = showDirectorAverage filmDB director
	if (isNaN average) then return 0 else return average

-- Gets user input for rating a film 
-- Checks title matches a film title in the database (film exists)
-- Validates rating is numerical and between 0 and 10
-- Returns new database with added ratings
rateFilmIO :: [Film] -> User -> IO [Film]
rateFilmIO filmDB user	= do
	putStr "                    Film Title: "
	title <- getLine
	if (filmExists filmDB title) 
		then do
            putStr "                        Rating: "
            r <- getLine
            if ((isValidRating r) && (length r <= 3))
                then do
                    let rating = (read r :: Float)
                    if (rating >= 0 && rating <= 10)
                        then do
                            let newFilmDB = rateFilm filmDB title (user, rating)
                            putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n" 
                            putStrLn "                               Rating Added!"
                            putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
                            return newFilmDB
                        else do
                            putStrLn "\n-------------------------------------------------------------------------------\n"
                            putStrLn "                          Rating not successful!"
                            putStrLn "           Please enter a valid rating between 0 and 10 inclusive! "
                            putStrLn "\n-------------------------------------------------------------------------------\n"
                            rateFilmIO filmDB user
                else do
                    putStrLn "\n-------------------------------------------------------------------------------\n"
                    putStrLn "                          Rating not successful!"
                    putStrLn "       Ensure string is in a valid numerical format (e.g. 1, 5.3, 9, 2.1,...)"
                    putStrLn "\n-------------------------------------------------------------------------------\n"
                    rateFilmIO filmDB user
        else do
            putStrLn "\n-------------------------------------------------------------------------------\n"
            putStrLn " Film does not exist in the database!"
            putStrLn "\n-------------------------------------------------------------------------------"
            return filmDB

-- Gets year from user input
-- Validates year is numerical and of correct length
-- Passes year to showRatingOrderedYearFilms to get films released after year and ordered by ratings (desc)
-- Prints result of function call (using showFilms to format string)
showYearFilmsIO :: [Film] -> IO ()
showYearFilmsIO filmDB	= do
	putStr " Year Released: "
	year <- getLine
	if ((isValidYear year))
		then do
			putStrLn (showFilms (showRatingOrderedYearFilms filmDB (read year :: Int)))
		else do
            putStrLn "\n You must enter a valid full year as a number (e.g. 2013)\n"
            showYearFilmsIO filmDB
	
-- Saves database back to file
saveAndQuit :: [Film] -> IO ()
saveAndQuit filmDB  = do
    writeFile "films.txt" (show filmDB)
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n" 
    putStrLn "                               Changes Saved!"
    putStrLn "\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
    putStrLn " Goodbye! \n"
    
        
        