module KMonad.NoOverlapInButtonNamingSpec (spec) where

import KMonad.Keyboard.Keycode
import qualified KMonad.Util.MultiMap as Q

import qualified RIO.NonEmpty as N

spec :: Spec
spec = describe "button-naming" $ do
  it "No duplicate button names" $ (filter (not . null . N.tail) . N.groupAllWith fst) (Q.reverse keyNames ^.. Q.itemed) `shouldBe` []
