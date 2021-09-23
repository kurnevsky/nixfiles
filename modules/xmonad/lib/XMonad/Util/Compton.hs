{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.Compton
-- Copyright   : (C) 2018 Evgeny Kurnevsky
-- License     : AGPL
--
-- Maintainer  : Evgeny Kurnevsky <kurnevsky@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A module for interaction with compton's dbus interface which allows to invert
-- colors of windows on the fly.
--
--------------------------------------------------------------------------------

module XMonad.Util.Compton ( getInversionStatus
                           , setInversionStatus
                           , invert
                           ) where

import Data.Maybe
import Data.Word
import DBus
import DBus.Client
import XMonad

dpyName :: Display -> String
dpyName = map replace . displayString where
  replace ':' = '_'
  replace '.' = '_'
  replace c = c

comptonInterface :: InterfaceName
comptonInterface = "com.github.chjj.compton"

comptonBus :: Display -> BusName
comptonBus dpy = busName_ $ "com.github.chjj.compton." ++ dpyName dpy

getInversionStatus :: Display -> Window -> IO (Maybe Bool)
getInversionStatus dpy w =
  let mc = (methodCall "/" comptonInterface "win_get")
             { methodCallDestination = Just $ comptonBus dpy
             , methodCallBody = [ toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                ]
             }
      extractStatus :: [Variant] -> Maybe Bool
      extractStatus [v] = fmap (== (1 :: Word32)) $ fromVariant v
      extractStatus _ = Nothing
  in do client <- connectSession
        status <- call_ client mc
        disconnect client
        return $ extractStatus $ methodReturnBody status

setInversionStatus :: Display -> Window -> Bool -> IO ()
setInversionStatus dpy w status =
  let mc = (methodCall "/" comptonInterface "win_set")
             { methodCallDestination = Just $ comptonBus dpy
             , methodCallBody = [ toVariant (fromIntegral w :: Word32)
                                , toVariant ("invert_color_force" :: String)
                                , toVariant ((if status then 1 else 0) :: Word32)
                                ]
             }
  in do client <- connectSession
        callNoReply client mc
        disconnect client

invert :: Display -> Window -> IO ()
invert dpy w =
  getInversionStatus dpy w >>= flip whenJust (setInversionStatus dpy w . not)
