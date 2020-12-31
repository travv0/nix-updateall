{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Shelly

main :: IO ()
main = shelly $ do
    Just home <- get_env "HOME"
    cd $ home </> ("nixos" :: FilePath)

    mgit <- which "git"
    case mgit of
        Nothing -> return ()
        Just git -> run_ git ["pull"]

    run_
        "sudo"
        [ "nix-channel"
        , "--add"
        , "https://nixos.org/channels/nixos-unstable"
        , "nixos"
        ]
    run_
        "nix-channel"
        [ "--add"
        , "https://github.com/nix-community/home-manager/archive/master.tar.gz"
        , "home-manager"
        ]

    run_ "sudo" ["nix-channel", "--update"]
    run_ "nix-channel" ["--update"]

    run_ "sudo" ["nix-collect-garbage", "--delete-older-than", "30d"]
    run_ "sudo" ["nixos-rebuild", "switch", "--upgrade"]
    run_ "home-manager" ["switch"]

    case mgit of
        Nothing -> return ()
        Just git -> do
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
