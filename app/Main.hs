{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Shelly
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    shelly $ print_commands ("-v" `elem` args) updateAll

updateAll :: Sh ()
updateAll = do
    Just home <- get_env "HOME"
    cd $ home </> ("nixos" :: FilePath)

    withGitMaybe $ \git -> run_ git ["pull"]

    runSudo_
        "nix-channel"
        [ "--add"
        , "https://nixos.org/channels/nixos-unstable"
        , "nixos"
        ]
    run_
        "nix-channel"
        [ "--add"
        , "https://github.com/nix-community/home-manager/archive/master.tar.gz"
        , "home-manager"
        ]

    runSudo_ "nix-channel" ["--update"]
    run_ "nix-channel" ["--update"]

    runSudo_ "nix-collect-garbage" ["--delete-older-than", "30d"]
    runSudo_ "nixos-rebuild" ["switch", "--upgrade"]
    run_ "home-manager" ["switch"]

    withGitMaybe $ \git -> do
        run_ git ["add", "-A"]
        setenv "LANG" "C.UTF-8"
        commitMsg <-
            run "git" ["-c", "color.status=false", "status"]
                -|- run
                    "sed"
                    [ "-n"
                    , "-r"
                    , "-e"
                    , "1,/Changes to be committed:/ d"
                    , "-e"
                    , "1,1 d"
                    , "-e"
                    , "/^Untracked files:/,$ d"
                    , "-e"
                    , "s/^\\s*//"
                    , "-e"
                    , "/^\\(/ d"
                    , "-e"
                    , "/./p"
                    ]
        unless (T.null commitMsg) $ do
            run_ git ["commit", "-m", commitMsg]
            run_ git ["push"]

withGitMaybe :: (FilePath -> Sh ()) -> Sh ()
withGitMaybe f = do
    mgit <- which "git"
    case mgit of
        Just git -> f git
        Nothing -> return ()

runSudo_ :: FilePath -> [Text] -> Sh ()
runSudo_ fp args = run_ "sudo" (toTextIgnore fp : args)
