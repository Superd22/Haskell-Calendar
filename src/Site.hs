{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector as V
import           Data.String
import           Control.Monad
import qualified Text.Digestive.Heist as D
import qualified Text.Digestive.Snap as D
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
import           Application
import           Calendar
import qualified Event as E 



data Event = Event -- Used in Splices 
    {
      title :: T.Text
    , hour :: T.Text
    , minutes :: T.Text
    , username :: T.Text
    } deriving (Show)

instance FromRow Event where
    fromRow = Event <$> field <*> field <*> field <*> field



---------------------------------------------------------------------------------
-- |                                                                         | -- 
-- |                                                                         | --
-- |                                LOGIN                                    | --
-- |                                                                         | --
-- |                         This is pretty much the                         | --
-- |                         code you get by creating                        | --
-- |                         a new Snap project, with                        | --
-- |                         modifications                                   | --
-- |                                                                         | --
-- |                                                                         | --
---------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit = do
    withTop sess $ setInSession "month" "8"
    withTop sess $ setInSession "year" "2014"
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/calendar")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"



------------------------------------------------------------------------------
-- | Render new user form
handleRenderNewUser :: Maybe T.Text -> Handler App (AuthManager App) ()
handleRenderNewUser existUser = heistLocal (I.bindSplices errs) $ render "new_user"
    where 
        errs = maybe noSplices splice existUser
        splice err = "existingUser" ## I.textSplice err

------------------------------------------------------------------------------
-- | Handle new user check
handleCheckNewUser :: Handler App (AuthManager App) ()
handleCheckNewUser = 
    getPostParam "login" >>= maybe (redirect "/new_user") checkUser
    where
        checkUser uname = do
            c <- usernameExists $ T.decodeUtf8 uname
            case c of
                True -> handleRenderNewUser $ Just "This username already exists. Please choose another one."
                False -> do
                    registerUser "login" "password"
                    redirect "/"

------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleCheckNewUser
  where
    handleForm = handleRenderNewUser Nothing 


------------------------------------------------------------------------------
-- | Checks if a user is authentified before accessing the given page, otherwise go back to root.
checkAuth :: Handler App (AuthManager App) () -> Handler App App ()
checkAuth handler = with auth $ requireUser auth (redirect "/") handler



---------------------------------------------------------------------------------
-- |                                                                         | --
-- |                                 CALENDAR                                | --
-- |                                                                         | --
---------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Handle the Calendar -> get the current month and year and pass it to the renderer. 
calHandler :: Handler App (AuthManager App) ()
calHandler = do m <- withTop sess $ getFromSession "month" 
                y <- withTop sess $ getFromSession "year"
                calRenderHandler (fromJust m) (fromJust y)

------------------------------------------------------------------------------
-- | Get the events and the calendar for this month and render the Calendar. 
-- | List of events is converted before being passed to monthSplice -> Allows easy indexing later. 
calRenderHandler :: T.Text -> T.Text  -> Handler App (AuthManager App) ()
calRenderHandler mon yea = do
    cuser <- currentUser
    events <- genEventDay 1 (gregorianMonthLength y m) mon yea
    renderWithSplices "/calendar" ("calendar" ## (monthSplice ds mon yea  (userLogin $ fromJust cuser) (V.fromList events)))
    where m = read $ T.unpack mon
          y = read $ T.unpack yea
          c = generateCal y m
          ds = days c


------------------------------------------------------------------------------
-- | Splice to display each day of the week in a month.
monthSplice :: [[Int]] -> T.Text -> T.Text -> T.Text -> V.Vector [Event] -> I.Splice (AppHandler)
monthSplice d m y u le = I.runChildrenWith (do "week"  ## I.mapSplices weekSplice d
                                               "month" ## I.textSplice my)
    where mi = read $ T.unpack m
          mn = getMonthName mi
          my = mappend mn (mappend " " y)
          weekSplice w  =  I.runChildrenWith ("data" ## I.mapSplices daySplice w)
          daySplice day = I.runChildrenWith $ splice 
             where  splice  = if day == 0
                                 then "day" ## I.textSplice ""
                                 else do "day" ## I.textSplice $ T.pack . show $ day
                                         when (not $ V.null le) $ do "event" ## I.mapSplices (eventSplice u) $ le V.! (day - 1)

------------------------------------------------------------------------------
-- | Splice to display every event of day.
eventSplice :: Monad m => T.Text -> Event -> I.Splice m
eventSplice u e = I.runChildrenWith $ do "etitle" ## I.textSplice $ title e
                                         "arrow"  ## I.textSplice $ " -> "
                                         "hour" ## I.textSplice $  hour e
                                         "minutes" ## I.textSplice $ minutes e
                                         "sep" ## I.textSplice $ ":"
                                         "delete" ## deleteSplice
    where deleteSplice = if u == username e
                            then return $ [X.Element "a" [("href", T.concat $ ["/delete/", title e])] [X.TextNode "  (X)"]]
                            else I.textSplice $ ""


    
------------------------------------------------------------------------------
-- | Handler to change the current month of year of the Calendar
changeCalHandler :: Handler App (AuthManager App) ()
changeCalHandler = do param <- getParam "month"
                      m <- withTop sess $ getFromSession "month" 
                      y <- withTop sess $ getFromSession "year"
                      case (fromJust param) of
                        "pyear" -> withTop sess $ setInSession "year" (changeDate (-1) (fromJust y))
                        "pmonth" -> case (fromJust m) of "1" -> do  withTop sess $ setInSession "month" "12"
                                                                    withTop sess $ setInSession "year" (changeDate (-1) (fromJust y))
                                                         _ -> withTop sess $ setInSession "month" (changeDate (-1) (fromJust m))
                        "nmonth" -> case (fromJust m) of "12" -> do  withTop sess $ setInSession "month" "1"
                                                                     withTop sess $ setInSession "year" (changeDate 1 (fromJust y))
                                                         _ ->  withTop sess $ setInSession "month" (changeDate 1 (fromJust m))
                        "nyear" -> withTop sess $ setInSession "year" (changeDate 1 (fromJust y))
                      withTop sess $ commitSession
                      redirect "/calendar"


------------------------------------------------------------------------------
-- | Auxiliary function to change the year of a date.
changeDate:: Int -> T.Text -> T.Text
changeDate m d = let dInt = read $ T.unpack d
                     cInt = dInt + m
                   in T.pack $ show cInt

------------------------------------------------------------------------------
-- | Bindings needed to form the query used to fetch all the events of day.
headQuery :: String
headQuery = "SELECT title, hour, minutes, username FROM events WHERE "
onceQuery :: String
onceQuery = "(repeat='Once' and day= ? and month= ? and year= ?) or "
dailyQuery :: String
dailyQuery = "repeat='Daily' or "
weeklyQuery :: String
weeklyQuery = "(repeat='Weekly' and weekday = ?) or "
monthlyQuery :: String
monthlyQuery = "(repeat= 'Monthly' and day = ?) or "
yearlyQuery :: String
yearlyQuery = "(repeat= 'Yearly' and day = ? and month = ? )"
sqlQuery :: Query
sqlQuery = fromString $ headQuery ++ onceQuery ++ dailyQuery ++ weeklyQuery ++ monthlyQuery ++ yearlyQuery 

------------------------------------------------------------------------------
-- |Generates all the events for each day in a month.
genEventDay :: Int -> Int -> T.Text -> T.Text -> Handler App (AuthManager App) [[Event]]
genEventDay day total m y = if (day > total)
                            then return []
                            else do once <- withTop db $ query sqlQuery (dstr, m, y, T.pack $ show wkday, dstr, dstr, m)
                                    (liftM2 (:)) (return once) $ genEventDay (day +1) total m y
        where dstr = T.pack $ show day
              mon = read $ T.unpack m
              yea = read $ T.unpack y
              (_,_,wkday) = toWeekDate $ fromGregorian yea mon day


------------------------------------------------------------------------------
-- |Handles the deletions of an Event.
deleteHandler :: Handler App Postgres ()
deleteHandler = do event <- getParam "event"
                   execute "DELETE FROM events WHERE title=?" [fromJust event]
                   redirect "/calendar"







---------------------------------------------------------------------------------
-- |                                                                         | --
-- |                              Event Form                                 | --
-- |                                                                         | --
---------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |Handles the rendering of the Event Form
eventFormHandler :: Handler App (AuthManager App) ()
eventFormHandler = do
    (view,result) <- D.runForm "eventform" E.eventForm 
    case result of
        Just event -> addEventHandler event        
        Nothing -> heistLocal (D.bindDigestiveSplices view) $ render "add_event"

------------------------------------------------------------------------------
-- |Handles the addition of a new Event.
addEventHandler :: E.Event -> Handler App (AuthManager App) ()
addEventHandler event = do 
    cuser <- currentUser
    withTop db $ execute "INSERT INTO events VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" (E.title event, d, m, y, h, minu, w, E.repeat event, userLogin $ fromJust cuser)
    redirect "/calendar"
    where d = E.day $ E.date event
          m = E.month $ E.date event
          y = E.year $ E.date event
          h = E.hour $ E.time event
          minu = E.minutes $ E.time event
          (_,_,w) = toWeekDate $ fromGregorian y m d


---------------------------------------------------------------------------------
-- |                                                                         | --
-- |                              Application                                | --
-- |                                                                         | --
---------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/calendar", checkAuth calHandler)
         , ("/calendar/:month", checkAuth changeCalHandler)
         , ("/add_event", checkAuth eventFormHandler)
         , ("/delete/:event", with db deleteHandler)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Calendar App" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
--    addConfig h $ mempty { hcCompiledSplices = calendarSplices}
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db pgsInit
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    addAuthSplices h auth
    return $ App h s d a 

