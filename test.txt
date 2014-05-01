-- MATHFUN - DISCRETE MATHEMATICS AND FUNCTIONAL PROGRAMMING
-- Functional Programming Assignment, 2012/13
-- Student Number: 630745


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#  DATA TYPES  #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Film datatype consisting of Title, Cast, Year, and Fans.
data Film =  Film String [String] Int [String]
			 deriving (Eq, Ord, Show, Read)


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#  FUNCTIONAL  CODE  #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Adds a new film at the end of the list of films provided.
fnAddNewFilm :: Film -> [Film] -> [Film]
fnAddNewFilm filmToAdd listOfFilms = listOfFilms ++ [filmToAdd]

-- Checks the year for a single film and returns whether it was released during
-- that year.
fnCheckFilmByYear :: Int -> Film -> Bool
fnCheckFilmByYear yearRequested (Film _ _ year _)
	| year == yearRequested = True
	| otherwise				= False

-- Gets a list of films and filters out the ones that were not released during
-- that year.
fnDisplayFilmsByYear yearRequested = filter (fnCheckFilmByYear yearRequested)

-- Goes through all the films and gets the one that matches the title given
-- and returns the rest of the film's values.
fnGetFilmByTitle :: String -> [Film] -> Film
fnGetFilmByTitle _ [] = (Film "" [] 0 [])
fnGetFilmByTitle name ((Film title cast year fans):films)
	| name == title = (Film title cast year fans)
	| otherwise		= fnGetFilmByTitle name films

-- Checks the fans in a single film and returns whether the specified fan liked
-- that film.
fnCheckFilmByFan :: String -> Film -> Bool
fnCheckFilmByFan fanRequested (Film title cast year (fan:fans))
	| fan == fanRequested = True
	| fans == []		  = False
	| otherwise			  = fnCheckFilmByFan fanRequested (Film title cast year fans)

-- Gets a list of films and filters out the ones that were not liked by a
-- specified fan.
fnDisplayFilmsByFan fanRequested = filter (fnCheckFilmByFan fanRequested)

-- Checks the actors in a single film and returns whether the specified actor
-- starred in that film.
fnCheckFilmByActor :: String -> Film -> Bool
fnCheckFilmByActor actorRequested (Film title (actor:cast) year fans)
	| actor == actorRequested = True
	| cast == []			  = False
	| otherwise				  = fnCheckFilmByActor actorRequested (Film title cast year fans)

-- Gets an actor name and a database of films and checks if the actor starred
-- in any of those films.
fnActorExists :: String -> [Film] -> Bool
fnActorExists _ [] = False
fnActorExists actorRequested (film:films)
	| fnCheckFilmByActor actorRequested film = True
	| otherwise							   = fnActorExists actorRequested films

-- Gets a list of films and filters out the ones that the specified actor did
-- not star in.
fnDisplayFilmsByActor actorRequested = filter (fnCheckFilmByActor actorRequested)

-- Checks if the film was released within the period of the start and end year.
fnCheckFilmInPeriod :: Int -> Int -> Film -> Bool
fnCheckFilmInPeriod startYear endYear (Film _ _ year _)
	| year >= startYear && year <= endYear = True
	| otherwise							   = False

-- Gets a list of films and filters out the ones that were not released during
-- the specified time period and the ones where the specified actor did not
-- star in.
fnDisplayFilmsByActorInPeriod actorRequested startYear endYear = filter (fnCheckFilmByActor actorRequested) . filter (fnCheckFilmInPeriod startYear endYear)

-- Gets the user's name from IO and the film name that the user wants to be a
-- fan of and adds the user as a fan of that film.
fnAddNewFan :: String -> String -> [Film] -> [Film]
fnAddNewFan _ _ [] = []
fnAddNewFan fan film ((Film title cast year fans):films)
	| film == title = (Film title cast year (fan:fans)) : (fnAddNewFan fan film films)
	| otherwise		= (Film title cast year fans) : (fnAddNewFan fan film films)

-- Gets a film and returns the number of its fans.
fnNumberOfFans (Film _ _ _ fans) = length fans

-- Gets a database of films and the first film in the database and checks with
-- that to find the film with the most fans, and returns it.
fnFilmWithMostFans :: [Film] -> Film -> Film
fnFilmWithMostFans [] maxFans = maxFans
fnFilmWithMostFans (film:films) maxFans
	| (fnNumberOfFans film) > (fnNumberOfFans maxFans) = fnFilmWithMostFans films film
	| otherwise									   = fnFilmWithMostFans films maxFans

-- Gets an actor name and a database of films and checks if the actor requested
-- has starred in a film. Checks the films they starred in for the one with the
-- most fans, otherwise it gets the one film with the most fans.
fnBestFilmByActor :: String -> [Film] -> Film
fnBestFilmByActor actorRequested (film:films)
	| fnActorExists actorRequested films = fnFilmWithMostFans (fnDisplayFilmsByActor actorRequested films) film
	| otherwise						   = Film "" [] 0 []

-- Gets the database of films twice and creates a list of the returned films
-- in order to avoid duplicating films from the top films function.
fnRemoveRepeatingFilms :: [Film] -> [Film] -> [Film]
fnRemoveRepeatingFilms _ [] = []
fnRemoveRepeatingFilms (originalFilm:originalFilms) (returnedFilm:returnedFilms)
	| returnedFilm == fnFilmWithMostFans originalFilms originalFilm = fnRemoveRepeatingFilms (originalFilm:originalFilms) returnedFilms
	| otherwise													  = returnedFilm : fnRemoveRepeatingFilms (originalFilm:originalFilms) returnedFilms

-- Gets a number of top films to display and a database and creates a new list
-- in order to display those films in ascending order (to be reversed in IO).
fnTopFilms :: Int -> [Film] -> [Film]
fnTopFilms 0 _ = []
fnTopFilms numberOfFilms (film:films) = (fnFilmWithMostFans films film) : (fnTopFilms (numberOfFilms - 1) (fnRemoveRepeatingFilms (film:films) (film:films)))

-- Reverses a list of films.
fnReverseList films = reverse films


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#   IO  CODE   #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Opens the films.txt file to load the films database.
ioLoadFile :: IO [Film]
ioLoadFile = do
	allFilms <- readFile "films.txt"
	let films = read allFilms :: [Film]
	return films

-- Gets the database that was passed from ioloadFile and the user's name and
-- passes them up to the displayFilms function.
ioGetName :: [Film] -> IO ()
ioGetName films = do
	putStrLn ""
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn "--#"
	putStr "--#  User name: "
	name <- getLine
	putStrLn "--#"
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	if name == ""
		then ioGetName films
		else ioDisplayMenu films name

-- Gets a list of strings and displays it separated by commas up until the
-- penultimate value.
ioDisplayList :: [String] -> IO ()
ioDisplayList [] = do return ()
ioDisplayList (item:list) = do
	putStr item
	if list /= []
		then putStr ", "
		else return ()
	ioDisplayList list

-- Displays one film on the screen.
ioIndividualFilm :: Film -> IO ()
ioIndividualFilm (Film title cast year fans) = do
	putStrLn ""
	putStrLn "-----------------------------------------------------------------------------"
	putStr "--  Title: "
	putStrLn title
	putStr "--  Cast: "
	ioDisplayList cast
	putStrLn ""
	putStr "--  Year: "
	putStrLn (show year)
	putStr "--  Fans: "
	putStrLn (show (fnNumberOfFans (Film title cast year fans)))
	putStrLn "-----------------------------------------------------------------------------"

-- Displays films from the list of films given.
ioDisplayFilms :: [Film] ->IO ()
ioDisplayFilms (film:films) = do
	ioIndividualFilm film
	if films /= []
		then ioDisplayFilms films
		else return ()

-- Attempts to get a string and keeps asking for it if it gets a null value.
ioGetString :: IO String
ioGetString = do
	str <- getLine
	if str == ""
		then ioGetString
		else return str

-- Loops through to get all the cast members until it reaches an empty string.
ioGetCast :: Int -> [String] -> IO [String]
ioGetCast numberOfActors fans = do
	putStr "--  Actor "
	putStr (show numberOfActors)
	putStr ": "
	fan <- getLine
	if fan /= ""
		then ioGetCast (numberOfActors + 1) (fan:fans)
		else return fans

-- Loops through to get all the film fans until it reaches an empty string.
ioGetFans :: Int -> [String] -> IO [String]
ioGetFans numberOfFans fans = do
	putStr "--  Fan "
	putStr (show numberOfFans)
	putStr ": "
	fan <- getLine
	if fan /= ""
		then ioGetFans (numberOfFans + 1) (fan:fans)
		else return fans

-- Saves the changes made to the database of films. Uses a short addition to
-- the original writeFile code in order to force readFile to read the entire
-- file and close it before the writeFile action can begin. This is done so
-- that the user can start the program and close it immediately without having
-- to have made any changes.
ioSaveChanges :: [Film] -> String -> IO ()
ioSaveChanges films name = do
	length films `seq` writeFile "films.txt" (show films)
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn "--"
	putStrLn "--  All changes saved."
	putStrLn "--"
	putStrLn "-----------------------------------------------------------------------------"
	ioDisplayMenu films name

-- Exits the program.
ioExit :: IO ()
ioExit = do
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn "--"
	putStrLn "-- Student ID: 630745"
	putStrLn "-- We <3 Haskell"
	putStrLn "--"
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn ""


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#   THE  EIGHT  FUCTIONS   #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Adds a film into the database and diplays an appropriate message.
ioAddFilm :: [Film] -> String -> IO ()
ioAddFilm films name = do
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn "--"
	putStr "--  Film title: "
	title <- ioGetString
	putStrLn "--  "
	putStr "--  Cast (empty to finish): "
	putStrLn ""
	cast <- ioGetCast 1 []
	putStrLn "--  "
	putStr "--  Release year: "
	year <- ioGetString
	putStrLn "--  "
	putStr "--  Fans (empty to finish): "
	putStrLn ""
	fans <- ioGetFans 1 []
	putStrLn "--"
	putStrLn "-----------------------------------------------------------------------------"
	let newFilms = fnAddNewFilm (Film title cast (read year :: Int) fans) films
	putStrLn ""
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn "--#"
	putStrLn "--#  Film successfully added"
	putStrLn "--#"
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn ""
	ioDisplayMenu newFilms name

-- Initialiser for the ioDisplayFilms function. Used to avoid overwriting the
-- original films with the new data.
initDisplayFilms :: [Film] -> String -> IO ()
initDisplayFilms films name = do
	putStrLn ""
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn "--#"
	putStrLn "--#  Displaying all films"
	putStrLn "--#"
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	ioDisplayFilms films
	putStrLn ""
	ioDisplayMenu films name

-- Gets a list of films and a year and displays the ones that were released
-- during that year.
ioFilmsByYear :: [Film] -> String -> IO ()
ioFilmsByYear films name = do
	putStr "Release year: "
	year <- ioGetString
	let fetchedFilms = fnDisplayFilmsByYear (read year :: Int) films
	if fetchedFilms == []
		then do
			putStrLn "-----------------------------------------------------------------------------"
			putStrLn "--"
			putStrLn "--  No films were released during this year."
			putStrLn "--"
			putStrLn "-----------------------------------------------------------------------------"
		else ioDisplayFilms fetchedFilms
	ioDisplayMenu films name

-- Gets a list of films and a fan name and displays the ones that the fan likes.
ioFilmsByFan :: [Film] -> String -> IO ()
ioFilmsByFan films name = do
	putStr "Fan name: "
	fan <- ioGetString
	let fetchedFilms = fnDisplayFilmsByFan fan films
	if fetchedFilms == []
		then do
			putStrLn "-----------------------------------------------------------------------------"
			putStrLn "--"
			putStrLn "--  This fan isn't a fan of any films."
			putStrLn "--"
			putStrLn "-----------------------------------------------------------------------------"
		else ioDisplayFilms fetchedFilms
	ioDisplayMenu films name

-- Gets a list of films, an actor name, a start data and an end date, and shows
-- the films that the specified actor starred in and that were released during
-- that period.
ioFilmsByActorInPeriod :: [Film] -> String -> IO ()
ioFilmsByActorInPeriod films name = do
	putStr "Actor name: "
	actor <- ioGetString
	putStr "Start year: "
	start <- ioGetString
	putStr "End year: "
	end <- ioGetString
	let fetchedFilms = fnDisplayFilmsByActorInPeriod actor (read start :: Int) (read end :: Int) films
	if fetchedFilms == []
		then do
			putStrLn "-----------------------------------------------------------------------------"
			putStrLn "--"
			putStrLn "-- No films were released during this period that this actor has starred in."
			putStrLn "--"
			putStrLn "-----------------------------------------------------------------------------"
		else ioDisplayFilms fetchedFilms
	ioDisplayMenu films name

-- Adds a fan to a film and gives an appropriate message if the user is already
-- a fan of that film.
ioAddFan :: [Film] -> String -> IO ()
ioAddFan films name = do
	putStr "Film name: "
	film <- ioGetString
	let filmValues = (fnGetFilmByTitle film films)
	if filmValues /= (Film "" [] 0 [])
		then do
			if (fnCheckFilmByFan name filmValues) == True
				then do
					putStrLn "-----------------------------------------------------------------------------"
					putStrLn "--"
					putStrLn "--  You are already a fan of this film."
					putStrLn "--"
					putStrLn "-----------------------------------------------------------------------------"
					ioDisplayMenu films name
				else do
					let newFilms = fnAddNewFan name film films
					putStrLn "-----------------------------------------------------------------------------"
					putStrLn "--"
					putStrLn "--  You are now a fan of this film."
					putStrLn "--"
					putStrLn "-----------------------------------------------------------------------------"
					ioDisplayMenu newFilms name
		else do
			putStrLn "-----------------------------------------------------------------------------"
			putStrLn "--"
			putStrLn "--  Film doesn't exist."
			putStrLn "--"
			putStrLn "-----------------------------------------------------------------------------"
			ioDisplayMenu films name

-- Gets a list of films and an actor name and returns the best film that the
-- actor has starred in.
ioDisplayBestFilm :: [Film] -> String -> IO ()
ioDisplayBestFilm films name = do
	putStr "Actor name: "
	actor <- ioGetString
	let fetchedFilm = fnBestFilmByActor actor films
	if fetchedFilm == (Film "" [] 0 [])
		then do
			putStrLn "-----------------------------------------------------------------------------"
			putStrLn "--"
			putStrLn "--  Actor doesn't exist."
			putStrLn "--"
			putStrLn "-----------------------------------------------------------------------------"
		else ioIndividualFilm fetchedFilm
	ioDisplayMenu films name

-- Displays the overall top five films based on the number of fans, in
-- descending order.
ioDisplayTopFive :: [Film] -> String -> IO ()
ioDisplayTopFive films name = do
	putStrLn "The overall top five films are listed below in descending order of number of fans."
	let topFilms = fnReverseList (fnTopFilms 5 films)
	ioDisplayFilms topFilms
	ioDisplayMenu films name


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#  MENU  #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Acts as the entry point for the program and calls the loadFile function to
-- get the list of films in order to pass it to the getName function
main :: IO ()
main = do
	films <- ioLoadFile
	ioGetName films

-- Displays the main menu of actions to be chosen from.
ioDisplayMenu :: [Film] -> String -> IO ()
ioDisplayMenu films name = do
	putStrLn ""
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn "--#"
	putStrLn "--#  Main Menu"
	putStrLn "--#"
	putStrLn "--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--"
	putStrLn ""
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn "-- 1 - Add a new film"
	putStrLn "-- 2 - Display all films"
	putStrLn "-- 3 - Display all films that were released in a given year"
	putStrLn "-- 4 - Display all films that a given user is a fan of"
	putStrLn "-- 5 - Display all films of a given actor that were released during a period"
	putStrLn "-- 6 - Become a fan of a particular film"
	putStrLn "-- 7 - Display the best film for a given actor"
	putStrLn "-- 8 - Display the overall top five films sorted in descending order of fans"
	putStrLn "-- 9 - Save changes"
	putStrLn "-- 0 - Exit"
	putStrLn "-----------------------------------------------------------------------------"
	putStrLn ""
	putStr "Action: "
	action <- getLine
	putStrLn ""
	ioDoAction action films name

-- Calls the respective function for each action as chosen by the user.
ioDoAction :: String -> [Film] -> String -> IO ()
ioDoAction "1" films name = ioAddFilm films name
ioDoAction "2" films name = initDisplayFilms films name
ioDoAction "3" films name = ioFilmsByYear films name
ioDoAction "4" films name = ioFilmsByFan films name
ioDoAction "5" films name = ioFilmsByActorInPeriod films name
ioDoAction "6" films name = ioAddFan films name
ioDoAction "7" films name = ioDisplayBestFilm films name
ioDoAction "8" films name = ioDisplayTopFive films name
ioDoAction "9" films name = ioSaveChanges films name
ioDoAction "0" films name = ioExit
ioDoAction _ films name = ioDisplayMenu films name


{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#   DATABASE   #--#--#--#--#--#--|-}
{-|--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--#--|-}

-- Demo function to test basic functionality (without persistence - i.e.
-- testDatabase doesn't change and nothing is saved/loaded to/from file).
demo :: Int -> IO ()
demo 1 = ioDisplayFilms (fnAddNewFilm (Film "The Great Gatsby" ["Leonardo Dicaprio", "Tobey Maguire"] 2013 []) testDatabase)
demo 2 = ioDisplayFilms (testDatabase)
demo 3 = ioDisplayFilms (fnDisplayFilmsByYear 2012 testDatabase)
demo 4 = ioDisplayFilms (fnDisplayFilmsByActor "Zoe" testDatabase)
demo 5 = ioDisplayFilms (fnDisplayFilmsByActorInPeriod "Tom Hanks" 2000 2011 testDatabase)
demo 6 = ioDisplayFilms (fnAddNewFan "Zoe" "Forrest Gump" testDatabase)
demo 61 = ioDisplayFilms (fnAddNewFan "Zoe" "Inception" testDatabase)
demo 7 = ioIndividualFilm (fnBestFilmByActor "Tom Hanks" testDatabase)
demo 8 = ioDisplayFilms (fnReverseList (fnTopFilms 5 testDatabase))

-- Database of films to be used for demonstration of functional code.
testDatabase :: [Film]
testDatabase = [(Film "Casino Royale" ["Daniel Craig", "Eva Green", "Judi Dench"] 2006 ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
				(Film "Cowboys & Aliens" ["Harrison Ford", "Daniel Craig", "Olivia Wilde"] 2011 ["Bill", "Jo", "Garry", "Kevin", "Olga", "Liz"]),
				(Film "Catch Me If You Can" ["Leonardo DiCaprio", "Tom Hanks"] 2002 ["Zoe", "Heidi", "Jo", "Emma", "Liz", "Sam", "Olga", "Kevin", "Tim"]),
				(Film "Mamma Mia!" ["Meryl Streep", "Pierce Brosnan"] 2008 ["Kevin", "Jo", "Liz", "Amy", "Sam", "Zoe"]),
				(Film "Saving Private Ryan" ["Tom Hanks", "Matt Damon"] 1998 ["Heidi", "Jo", "Megan", "Olga", "Zoe", "Wally"]),
				(Film "Life of Pi" ["Suraj Sharma"] 2012 ["Kevin", "Olga", "Liz", "Tim", "Zoe", "Paula", "Jo", "Emma"]),
				(Film "Titanic" ["Leonardo DiCaprio", "Kate Winslet"] 1997 ["Zoe", "Amy", "Heidi", "Jo", "Megan", "Olga"]),
				(Film "Quantum of Solace" ["Daniel Craig", "Judi Dench"] 2008 ["Bill", "Olga", "Tim", "Zoe", "Paula"]),
				(Film "You've Got Mail" ["Meg Ryan", "Tom Hanks"] 1998 ["Dave", "Amy"]),
				(Film "Collateral" ["Tom Cruise", "Jamie Foxx"] 2004 ["Dave", "Garry", "Megan", "Sam", "Wally"]),
				(Film "The Departed" ["Leonardo DiCaprio", "Matt Damon", "Jack Nicholson"] 2006 ["Zoe", "Emma", "Paula", "Olga", "Dave"]),
				(Film "Inception" ["Leonardo DiCaprio"] 2010 ["Chris", "Emma", "Jo", "Bill", "Dave", "Liz", "Wally", "Zoe", "Amy", "Sam", "Paula", "Kevin", "Olga"]),
				(Film "Up in the Air" ["George Clooney", "Vera Farmiga"] 2009 ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
				(Film "The Shawshank Redemption" ["Tim Robbins", "Morgan Freeman"] 1994 ["Jo", "Wally", "Liz", "Tim", "Sam", "Zoe", "Emma", "Garry", "Olga", "Kevin"]),
				(Film "Gladiator" ["Russell Crowe", "Joaquin Phoenix"] 2000 ["Garry", "Ian", "Neal"]),
				(Film "The King's Speech" ["Colin Firth", "Geoffrey Rush"] 2010 ["Garry", "Megan", "Sam", "Ian", "Bill", "Emma", "Chris"]),
				(Film "The Descendants" ["George Clooney"] 2011 ["Wally", "Liz", "Kevin", "Tim", "Emma", "Chris", "Megan"]),
				(Film "Cloud Atlas" ["Tom Hanks", "Halle Berry"] 2012 ["Dave", "Amy", "Garry", "Ian", "Neal"]),
				(Film "The Reader" ["Kate Winslet", "Ralph Fiennes"] 2008 ["Emma", "Bill", "Dave", "Liz"]),
				(Film "Minority Report" ["Tom Cruise"] 2002 ["Dave", "Garry", "Megan", "Sam", "Wally"]),
				(Film "Revolutionary Road" ["Leonardo DiCaprio", "Kate Winslet"] 2008 ["Wally", "Sam", "Dave", "Jo"]),
				(Film "Forrest Gump" ["Tom Hanks"] 1994 ["Ian", "Garry", "Bill", "Olga", "Liz", "Sam", "Dave", "Jo", "Chris", "Wally", "Emma"]),
				(Film "Larry Crowne" ["Tom Hanks", "Julia Roberts"] 2011 ["Liz", "Wally"]),
				(Film "The Terminal" ["Tom Hanks", "Catherine Zeta Jones"] 2004 ["Olga", "Heidi", "Bill", "Sam", "Zoe"]),
				(Film "Django Unchained" ["Jamie Foxx", "Leonardo DiCaprio", "Christoph Waltz"] 2012 ["Kevin", "Tim", "Emma", "Olga"])]