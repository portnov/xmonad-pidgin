
module Main where

import XMonad
import XMonad.Pidgin

pidginConfig :: PidginWorkspaces
pidginConfig = PidginWorkspaces {
    moveBuddies = [(Groups ["haskell", "python"], Fixed "programming"), -- Move chats from 'haskell' and 'python' groups to 'programming' workspace
                   (AnyGroup, Corresponding) -- Move all other chats to workspace with name equal to Pidgin's group name
                   ],
    defaultWorkspace = "chats"
  }

main :: IO ()
main = do
  xmonad $ def {
    -- Workspaces list can include Pidgin's roster groups. Or corresponding workspaces
    -- will be created automatically.
    workspaces = ["main", "web", "chats", "programming", "xmonad"],
    -- Connect to Pidgin at startup
    startupHook = pidginConnect,
    -- Move Pidgin's windows to suitable workspaces
    manageHook = pidginMoveByGroup pidginConfig <+> manageHook def
  }
