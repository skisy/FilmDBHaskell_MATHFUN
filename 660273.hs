-- 
-- MATHFUN - Discrete Mathematics and Functional Programming
-- Functional Programming Assignment 2013/14 
-- Epic movie database 9001
-- 660273
--

import Data.List
import Text.Printf
--
-- Types
type Title      = String
type Director   = String
type Year       = Int
type User       = String
type Rating     = (User, Float)

data Film       = Film Title Director Year [Rating]
                  deriving(Eq,Ord,Show,Read)


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

-- 
--
-- FUNCTIONAL CODE
--
--

showFilm (Film title director year ratings) = "\nTitle: " ++ title ++ "\nDirector: " ++ director ++ "\nYear: " ++ show year ++ "\nRating: " ++ printf "%3.1f" (averageRating ratings)

average xs = sum xs / genericLength xs

averageRating :: [Rating] -> Float
averageRating ratings = average (map snd ratings)


-- i. ADD NEW FILM
-- ===============
addFilm :: Title -> Director -> Year -> [Film] -> [Film]
addFilm title director year oldDB =   oldDB ++ [Film title director year []]

-- ii. GIVE ALL FILMS
-- ==================
showFilms :: [Film] -> String
showFilms [] = "No films found!"
showFilms filmDB = foldr (++) "" (map showFilm filmDB)

-- iii. GIVE ALL FILMS BY DIRECTOR
-- ===============================
showDirectorFilms :: [Film] -> Director -> String
showDirectorFilms filmDB director = showFilms (filter(\(Film _ d _ r) -> d==director) filmDB)

-- iv. GIVE ALL FILMS WITH RATING >= 6
-- ===================================
showSixRatedFilms :: [Film] -> String
showSixRatedFilms filmDB = showFilms (filter(\(Film _ _ _ r) -> averageRating r >= 6) filmDB)

-- v. GIVE AVERAGE RATINGS FOR FILMS BY DIRECTOR
-- =============================================
--showDirectorAverage :: [Film] -> Director -> String
--showDirectorAverage filmDB director = 

-- vi. GIVE TITLES RATED BY USER WITH USERS RATINGS
-- ================================================
--showUserRated :: [Film] -> User -> [Film]
--showUserRated filmDB user = filter(\(Film _ _ _ r) -> (snd r) == user) filmDB

-- vii. ALLOW USER TO RATE/RE-RATE FILM
-- ====================================

-- viii. GIVE ALL FILMS RELEASED DURING OR AFTER YEAR, SORTED IN DESC ORDER OF RATING
-- ==================================================================================
--getYearFilms :: [Film] -> Year -> String
--getYearFilms filmDB year = showFilms (filter(\(Film _ _ y _) -> y>=year) filmDB)              How to get average before printing it in STRING

-- Demo function to test basic functionality (without persistence - i.e. 
-- testDatabase doesn't change and nothing is saved/loaded to/from file).

--demo :: Int -> IO ()
--demo 1  = putStrLn all films after adding 2013 film "Gravity" 
--                   by "Alfonso Cuaron" to testDatabase
--demo 2  = putStrLn (fnToTurnAListOfFilmsIntoAMultiLineString testDatabase)
--demo 3  = putStrLn all films by "James Cameron"
--demo 4  = putStrLn all films with website rating >= 6
--demo 5  = putStrLn average website rating for "James Cameron"
--demo 6  = putStrLn film titles and user ratings for "Zoe"
--demo 7  = putStrLn all films after Zoe rates "Jaws" 8
--demo 77 = putStrLn all films after Zoe rates "Vertigo" 3
--demo 8  = putStrLn "films from or after 2009 sorted by rating"

--
--
-- Your user interface code goes here
--
--
