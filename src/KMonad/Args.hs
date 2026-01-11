{-|
Module      : KMonad.Args
Description : How to parse arguments and config files into an AppCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Args
  ( getCmd, loadConfig, Cmd(..))
where

import KMonad.Prelude
import KMonad.App.Types
import KMonad.Args.Cmd
import KMonad.Args.Joiner
import KMonad.Args.Parser
import KMonad.Args.Types

import Control.Lens

--------------------------------------------------------------------------------
--

-- | Parse a configuration file into a 'AppCfg' record
loadConfig :: HasLogFunc e => Cmd -> RIO e AppCfg
loadConfig cmd = do

  tks <- loadTokens (cfgFile cmd)       -- This can throw a ParseError
  cgt <- joinConfigIO (joinCLI cmd tks) -- This can throw a JoinError

  -- Try loading the sink and src
  lf  <- view logFuncL
  snk <- liftIO . snk cgt $ lf
  src <- liftIO . src cgt $ lf

  -- Emit the release of <Enter> if requested

  -- Assemble the AppCfg record
  pure $ AppCfg
    { _keySinkDev   = snk
    , _keySourceDev = src
    , _keymapCfg    = km      cgt
    , _firstLayer   = fstL    cgt
    , _fallThrough  = flt     cgt
    , _allowCmd     = allow   cgt
    , _startDelay   = strtDel cmd
    , _keyOutDelay  = fromIntegral <$> ksd cgt
    }


-- | Join the options given from the command line with the one read from the
-- configuration file.
-- This does not yet throw any kind of exception, as we are simply inserting the
-- given options into every 'KDefCfg' block that we see.
joinCLI :: Cmd -> [KExpr] -> [KExpr]
joinCLI Cmd{..} = traverse._KDefCfg %~ insertCliOption cliList
 where
  -- | All options and flags that were given on the command line.
  cliList :: DefSettings
  cliList = catMaybes $
       map flagToMaybe [cmdAllow, fallThrgh]
    <> [iToken, oToken, cmpSeq, cmpSeqDelay, keySeqDelay, implArnd]

  -- | Convert command line flags to a 'Maybe' type, where the non-presence, as
  -- well as the default value of a flag will be interpreted as @Nothing@
  flagToMaybe :: DefSetting -> Maybe DefSetting
  flagToMaybe = \case
    SAllowCmd    b -> if b then Just (SAllowCmd    b) else Nothing
    SFallThrough b -> if b then Just (SFallThrough b) else Nothing
    _              -> Nothing

  -- | Insert all command line options, potentially overwriting already existing
  -- options that were given in the configuration file. This is a paramorphism
  insertCliOption :: DefSettings -> DefSettings -> DefSettings
  insertCliOption cliSettings cfgSettings =
    foldr (\s cfgs ->
             if   s `elem` cfgs
             then map (\x -> if s == x then s else x) cfgs
             else s : cfgs)
          cfgSettings
          cliSettings
