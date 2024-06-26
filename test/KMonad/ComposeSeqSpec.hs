module KMonad.ComposeSeqSpec ( spec ) where

import KMonad.Prelude
import KMonad.Parsing
import KMonad.Args.Parser
import KMonad.Args.Types
import KMonad.Keyboard.ComposeSeq

import Test.Hspec hiding (around)

spec :: Spec
spec = describe "compose-sequences" $ traverse_ checkComposeSeq ssComposed
 where
  checkComposeSeq (_, c, name) =
    it ("Compose sequence for " <> unpack name <> " is valid") $
      runParser buttonP "" (pack [c]) `shouldSatisfy` parsesAsValidComposeSeq
  parsesAsValidComposeSeq (Right (KComposeSeq seq')) = all isSimple seq'
  parsesAsValidComposeSeq _ = False
  isSimple (KEmit _) = True
  isSimple (KAround b1 b2) = isSimple b1 && isSimple b2
  isSimple _ = False
