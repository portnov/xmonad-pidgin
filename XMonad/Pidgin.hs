{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

-- | XMonad integration with Pidgin IM.
module XMonad.Pidgin (
    -- * Data types
    PidginGroup (..),
    PidginWorkspaces (..),
    Target (..),
    -- * Startup hook
    pidginConnect,
    -- * Manage hook
    pidginMoveByGroup,
    -- * Utilities
    hasGroup,
    pidginGroup,
    togglePidginRoster
  ) where

import Control.Monad
import qualified Control.Exception as E
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe
import Data.Int
import Data.Typeable
import Data.Yaml
import System.Environment (getEnv)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.LayoutBuilder
import XMonad.Utils

import DBus
import DBus.Client
import DBus.TH

type Ints = [Int32]
type Buddies = M.Map String Int32
newtype SavedBuddies = SavedBuddies (M.Map String String)

instance ToJSON SavedBuddies where
  toJSON (SavedBuddies m) = Object $ H.fromListWith cat $ map swap $ M.assocs m
    where
      swap (v, k) = (T.pack k, Array $ V.fromList [String $ T.pack v])
      cat (Array v1) (Array v2) = Array (V.concat [v1, v2])

instance FromJSON SavedBuddies where
  parseJSON (Object m) = do
      pairs <- mapM unpack $ H.toList m
      return $ SavedBuddies $ M.fromList $ concat pairs
    where
      unpack (g, (Array ts)) = do
          titles <- mapM unpackTitle (V.toList ts)
          return [(t, T.unpack g) | t <- titles]

      unpack (g, x) = fail $ "SavedBuddies.fromJSON.unpack: invalid object in group " ++ T.unpack g ++ ": " ++ show x

      unpackTitle (String t) = return (T.unpack t)
      unpackTitle x = fail $ "SavedBuddies.fromJSON.unpackTitle: invalid object: " ++ show x

data PidginData =
     Pidgin {
       pidginProxy :: Client,
       pidginBuddies :: Buddies,
       pidginChats :: Buddies }
   | LoadedState { savedBuddies :: SavedBuddies }
   | NoDataLoaded
  deriving (Typeable)

type Pidgin = Maybe (TVar PidginData)

instance Show PidginData where
  show (Pidgin _ buddies chats) = "Pidgin <" ++ show buddies ++ "> <" ++ show chats ++ ">"
  show NoDataLoaded = "<no connection>"

-- | Pidgin's buddies\/chats groups specification
data PidginGroup = 
    AnyGroup                -- ^ Any group
  | Group String            -- ^ Only one named group
  | Groups [String]         -- ^ Only groups from the list
  deriving (Eq, Show, Read)

-- | Configuration of Pidgin's workspaces
data PidginWorkspaces =
    PidginWorkspaces {
      moveBuddies :: [(PidginGroup, Target)]  -- ^ Set of rules for Pidgin's conversation windows
    , defaultWorkspace :: WorkspaceId         -- ^ Default workspace for Pidgin's windows
    }

-- | Target workspace specification
data Target =
    Fixed WorkspaceId -- ^ Move to workspace with specfiied name
  | Corresponding     -- ^ Move to workspace, which name equals to Pidgin's buddies group name.

instance ExtensionClass Pidgin where
  initialValue = Nothing

interface "im.pidgin.purple.PurpleService"
    "/im/pidgin/purple/PurpleObject"
    "im.pidgin.purple.PurpleInterface" (Just "Purple")
    [ "AccountsFind" =:: ''String :-> ''String :-> Return ''Int32
    , "AccountsGetAllActive" =:: Return ''Ints
    , "FindBuddy" =:: ''Int32 :-> ''String :-> Return ''Int32
    , "BuddyGetGroup" =:: ''Int32 :-> Return ''Int32
    , "GroupGetName"  =:: ''Int32 :-> Return ''String
    , "BlistGetBuddies" =:: Return ''Ints
    , "BuddyGetAlias" =:: ''Int32 :-> Return ''String
    , "BlistSetVisible" =:: ''Int32 :-> Return ''Int32
    , "GetChats" =:: Return ''Ints
    , "ConversationGetTitle" =:: ''Int32 :-> Return ''String
    , "BlistGetRoot" =:: Return ''Int32
    , "BlistNodeNext" =:: ''Int32 :-> ''Int32 :-> Return ''Int32
    , "BlistNodeIsChat" =:: ''Int32 :-> Return ''Int32
    , "ChatGetName" =:: ''Int32 :-> Return ''String
    , "ChatGetGroup" =:: ''Int32 :-> Return ''Int32 ]

pidginGetChats :: Client -> IO Buddies
pidginGetChats dbus = do
    Just root <- blistGetRoot dbus
    list <- walk root
    return $ M.fromList list
  where
    walk :: Int32 -> IO [(String, Int32)]
    walk node = do
      result <- check node
      Just next <- blistNodeNext dbus node 0
      if next == 0
        then return result
        else do
             other <- walk next
             return (result ++ other)

    check node = do
      Just isChat <- blistNodeIsChat dbus node
      if isChat == 1
        then do
          Just title <- chatGetName dbus node
          return [(title, node)]
        else
          return []

-- | Connect to Pidgin via DBus. This function is to be called from @startupHook@.
-- NB: for this to work properly, you have to build xmonad with @-threaded@ RTS.
pidginConnect :: X ()
pidginConnect = do
  var <- io $ atomically $ newTVar NoDataLoaded
  io $ pidginConnector var
  XS.put (Just var)

stateFilePath :: IO FilePath
stateFilePath = do
  home <- getEnv "HOME"
  return $ home </> ".xmonad" </> "pidgin-groups.yaml"

pidginConnector :: TVar PidginData -> IO ()
pidginConnector var = do
    path <- stateFilePath
    ex <- doesFileExist path
    when ex $ do
        mbState <- decodeFile path
        whenJust mbState $ \m -> atomically $ writeTVar var (LoadedState m)
    forkIO connector
    return ()
  where
    matchRule = matchAny {
                  matchInterface = Just "im.pidgin.purple.PurpleInterface",
                  matchMember = Just "Quitting" }
    connector = do
      st <- atomically $ readTVar var
      case st of
        Pidgin {} -> do
             threadDelay (10 * 1000000)
             connector
        _ ->
             do
             x <- E.try connect
             case x of
                Right res -> do
                             atomically $ writeTVar var res
                             saveState var
                             listen (pidginProxy res) matchRule (onPidginQuit var)
                             putStrLn "Connected to Pidgin!"
                Left (err :: E.SomeException) ->
                  do
                  putStrLn $ "Error connecting to Pidgin: " ++ show err
                  threadDelay (10 * 1000000)
                  connector

    connect = do
      putStrLn "Trying to connect to Pidgin..."
      dbus <- connectSession
      putStrLn "DBus connect OK"
      Just buddiesL <- blistGetBuddies dbus
      putStrLn "buddies list got"
      buddies <- forM buddiesL $ \buddy -> do
                        Just name <- buddyGetAlias dbus buddy
                        --print name
                        return (name, buddy)
      putStrLn "buddies aliases got"
      chats <- pidginGetChats dbus
      putStrLn "chats got"
      let buddiesMap = M.fromList buddies
      putStrLn "Done"
      return $ Pidgin dbus buddiesMap chats

    onPidginQuit var signal = do
      putStrLn "Pidgin quits"
      atomically $ writeTVar var NoDataLoaded

    saveState var = do
      putStrLn "Saving state"
      st <- atomically $ readTVar var
      case st of
        Pidgin dbus buddies chats -> do
          buddiesToSave <- mapM (readBuddyGroup' dbus) (M.assocs buddies)
          chatsToSave <- mapM (readChatGroup' dbus) (M.assocs chats)
          path <- stateFilePath
          encodeFile path $ SavedBuddies $ M.fromList $ catMaybes $ buddiesToSave ++ chatsToSave

    readBuddyGroup' dbus (t,i) = do
      x <- readBuddyGroup dbus i
      case x of
        Nothing -> return Nothing
        Just grp -> return $ Just (t, grp)

    readChatGroup' dbus (t,i) = do
      x <- readChatGroup dbus i
      case x of
        Nothing -> return Nothing
        Just grp -> return $ Just (t, grp)

withPidgin :: (PidginData -> X (Maybe a)) -> X (Maybe a)
withPidgin fn = do
  mbv <- XS.get
  case mbv of
    Nothing -> return Nothing
    Just var -> do
      mbp <- io $ atomically $ readTVar var
      case mbp of
        NoDataLoaded -> return Nothing
        pidgin -> fn pidgin

-- | Toggle Pidgin's roster window visibility
togglePidginRoster :: X ()
togglePidginRoster = do
  withPidgin $ \pidgin ->
    case pidgin of
      Pidgin {} -> do
        rosters <- matchingWindows (role =? "buddy_list")
        let flag = if null rosters then 1 else 0
        io $ blistSetVisible (pidginProxy pidgin) flag
        return Nothing
      _ -> return Nothing
  return ()

-- | Get name of Pidgin's buddies group for conversation window.
-- Returns @Nothing@ for non-Pidgin's windows.
pidginGroup :: Query (Maybe String)
pidginGroup = do
  cls <- className
  if cls /= "Pidgin"
    then return Nothing
    else do
         ttl <- title
         liftX $ withPidgin $ \pidgin ->
           case pidgin of
             Pidgin dbus buddies chats ->
               case M.lookup ttl buddies of
                 Nothing -> pidginChatGroup dbus chats ttl
                 Just i  -> io $ readBuddyGroup dbus i
             LoadedState (SavedBuddies buddies) -> return $ M.lookup ttl buddies
             NoDataLoaded -> return Nothing

readBuddyGroup :: Client -> Int32 -> IO (Maybe String)
readBuddyGroup dbus i = do
  Just grp <- buddyGetGroup dbus i
  groupGetName dbus grp

readChatGroup :: Client -> Int32 -> IO (Maybe String)
readChatGroup dbus i = do
  Just grp <- chatGetGroup dbus i
  groupGetName dbus grp

pidginChatGroup :: Client -> Buddies -> String -> X (Maybe String)
pidginChatGroup dbus chats title = do
  case M.lookup title chats of
    Nothing -> return Nothing
    Just c -> io $ readChatGroup dbus c

-- | Move Pidgin's conversation window to suitable workspace.
pidginMoveByGroup :: PidginWorkspaces -> ManageHook
pidginMoveByGroup cfg = do
  grp <- pidginGroup
  case grp of
   Nothing -> doF id
   Just group ->
     createAndMove True Nothing (targetWorkspace cfg group)

targetWorkspace :: PidginWorkspaces -> String -> WorkspaceId
targetWorkspace cfg group = go (moveBuddies cfg)
  where
    go [] = defaultWorkspace cfg
    go ((AnyGroup, wksp):_) = getWS wksp
    go ((Group name, wksp):xs)
      | name == group = getWS wksp
      | otherwise     = go xs
    go ((Groups list, wksp):xs)
      | group `elem` list = getWS wksp
      | otherwise         = go xs
    
    getWS (Fixed name)  = name
    getWS Corresponding = group

-- | Check if window belongs to specified Pidgin's buddies group
hasGroup :: PidginGroup -> Query Bool
hasGroup AnyGroup = return True
hasGroup (Group name) = do
  grp <- pidginGroup
  return (grp == Just name)
hasGroup (Groups names) = do
  grp <- pidginGroup
  case grp of
    Nothing -> return False
    Just group -> return (group `elem` names)

instance Predicate PidginGroup Window where
  alwaysTrue _ = AnyGroup

  checkPredicate grp w = runQuery (hasGroup grp) w

