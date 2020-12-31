{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Shelly
import System.Environment (getArgs)

default (T.Text)

main :: IO ()
main = do
    args <- getArgs
    shelly $ print_commands ("-v" `elem` args) updateAll

updateAll :: Sh ()
updateAll = do
    home <- fromMaybe (error "HOME env var not set") <$> get_env "HOME"
    cd $ home </> "nixos"

    withBinMaybe "git" $ \git -> cmd git "pull"

    cmd
        "sudo"
        "nix-channel"
        "--add"
        "https://nixos.org/channels/nixos-unstable"
        "nixos" ::
        Sh ()

    cmd
        "nix-channel"
        "--add"
        "https://github.com/nix-community/home-manager/archive/master.tar.gz"
        "home-manager" ::
        Sh ()

    cmd "sudo" "nix-channel" "--update" :: Sh ()
    cmd "nix-channel" "--update" :: Sh ()

    cmd "sudo" "nix-collect-garbage" "--delete-older-than" "30d" :: Sh ()
    cmd "sudo" "nixos-rebuild" "switch" "--upgrade" :: Sh ()
    cmd "home-manager" "switch" :: Sh ()

    withBinMaybe "stack" $ \stack ->
        cmd
            stack
            "install"
            "fourmolu"
            "--stack-yaml"
            (toTextIgnore $ home </> ".stack/global-project/stack.yaml")

    withBinMaybe "git" $ \git -> do
        cmd git "add" "-A" :: Sh ()
        setenv "LANG" "C.UTF-8"
        commitMsg <-
            cmd git "-c" "color.status=false" "status"
                -|- cmd
                    "sed"
                    "-n"
                    "-r"
                    "-e"
                    "1,/Changes to be committed:/ d"
                    "-e"
                    "1,1 d"
                    "-e"
                    "/^Untracked files:/,$ d"
                    "-e"
                    "s/^\\s*//"
                    "-e"
                    "/^\\(/ d"
                    "-e"
                    "/./p"

        unless (T.null commitMsg) $ do
            cmd git "commit" "-m" commitMsg :: Sh ()
            cmd git "push"

withBinMaybe :: FilePath -> (FilePath -> Sh ()) -> Sh ()
withBinMaybe bin f = which bin >>= maybe (return ()) f
