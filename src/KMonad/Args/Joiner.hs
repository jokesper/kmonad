{-# LANGUAGE CPP #-}
{-|
Module      : KMonad.Args.Joiner
Description : The code that turns tokens into a DaemonCfg
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

We perform configuration parsing in 2 steps:
- 1. We turn the text-file into a token representation
- 2. We check the tokens and turn them into an AppCfg

This module covers step 2.

NOTE: This is where we make a distinction between operating systems.

-}
module KMonad.Args.Joiner
  ( joinConfigIO
  , joinConfig
  )
where

import KMonad.Prelude hiding (uncons)
import KMonad.Util

import KMonad.Args.Types

import KMonad.Model.Action
import KMonad.Model.Button
import KMonad.Keyboard
import KMonad.Keyboard.IO

#ifdef linux_HOST_OS
import KMonad.Keyboard.IO.Linux.DeviceSource
import KMonad.Keyboard.IO.Linux.UinputSink
#endif

#ifdef mingw32_HOST_OS
import KMonad.Keyboard.IO.Windows.LowLevelHookSource
import KMonad.Keyboard.IO.Windows.SendEventSink
#endif

#ifdef darwin_HOST_OS
import KMonad.Keyboard.IO.Mac.IOKitSource
import KMonad.Keyboard.IO.Mac.KextSink
#endif

import Control.Monad.Except

import RIO.List (headMaybe, intersperse, uncons, sort, group, find)
import RIO.Partial (fromJust)
import qualified KMonad.Util.LayerStack  as L
import qualified RIO.NonEmpty     as NE
import qualified RIO.HashMap      as M
import qualified RIO.Text         as T

--------------------------------------------------------------------------------
-- $err

-- | All the things that can go wrong with a joining attempt
data JoinError
  = DuplicateBlock   Text
  | MissingBlock     Text
  | DuplicateAlias   Text
  | CyclicAlias      Text
  | DuplicateLayer   Text
  | DuplicateSource  (Maybe Text)
  | DuplicateKeyInSource (Maybe Text) [Keycode]
  | MissingAlias     Text
  | MissingLayer     Text
  | MissingSource    (Maybe Text)
  | MissingSetting   Text
  | DuplicateSetting Text
  | DuplicateLayerSetting Text Text
  | InvalidOS        Text
  | ImplArndDisabled
  | NestedTrans
  | InvalidComposeKey
  | LengthMismatch   Text Int Int
  | NotAKeycode      DefButton

instance Show JoinError where
  show e = case e of
    DuplicateBlock    t   -> "Encountered duplicate block of type: " <> T.unpack t
    MissingBlock      t   -> "Missing at least 1 block of type: "    <> T.unpack t
    DuplicateAlias    t   -> "Multiple aliases of the same name: "   <> T.unpack t
    CyclicAlias       t   -> "Aliases references itself in a loop: " <> T.unpack t
    DuplicateLayer    t   -> "Multiple layers of the same name: "    <> T.unpack t
    DuplicateSource   t   -> case t of
      Just t  -> "Multiple sources of the same name: " <> T.unpack t
      Nothing -> "Multiple default sources"
    DuplicateKeyInSource   t ks   -> case t of
      Just t  -> "Keycodes appear multiple times in source `" <> T.unpack t <> "`:" <> ((' ' :) . show =<< ks)
      Nothing -> "Keycodes appear multiple times in default source: " <> ((' ' :) . show =<< ks)
    MissingAlias      t   -> "Reference to non-existent alias: "     <> T.unpack t
    MissingLayer      t   -> "Reference to non-existent layer: "     <> T.unpack t
    MissingSource     t   -> case t of
      Just t  -> "Reference to non-existent source: " <> T.unpack t
      Nothing -> "Reference to non-existent default source"
    MissingSetting    t   -> "Missing setting in 'defcfg': "         <> T.unpack t
    DuplicateSetting  t   -> "Duplicate setting in 'defcfg': "       <> T.unpack t
    DuplicateLayerSetting t s -> "Duplicate setting in 'deflayer '"  <> T.unpack t <> "': " <> T.unpack s
    InvalidOS         t   -> "Not available under this OS: "         <> T.unpack t
    ImplArndDisabled      -> "Implicit around via `A` or `S-a` are disabled in your config"
    NestedTrans           -> "Encountered 'Transparent' ouside of top-level layer"
    InvalidComposeKey     -> "Encountered invalid button as Compose key"
    LengthMismatch t l s  -> mconcat
      [ "Mismatch between length of 'defsrc' and deflayer <", T.unpack t, ">\n"
      , "Source length: ", show s, "\n"
      , "Layer length: ", show l ]
    NotAKeycode       b   -> "Encountered non keycode button in 'defsrc': " <> show b


instance Exception JoinError

-- | Joining Config
data JCfg = JCfg
  { _cmpKey  :: Button  -- ^ How to prefix compose-sequences
  , _implArnd :: ImplArnd -- ^ How to handle implicit `around`s
  , _kes     :: [KExpr] -- ^ The source expresions we operate on
  }
makeLenses ''JCfg

defJCfg :: [KExpr] ->JCfg
defJCfg = JCfg
  (emitB KeyRightAlt)
  IAAround

-- | Monad in which we join, just Except over Reader
newtype J a = J { unJ :: ExceptT JoinError (Reader JCfg) a }
  deriving ( Functor, Applicative, Monad
           , MonadError JoinError , MonadReader JCfg)

-- | Perform a joining computation
runJ :: J a -> JCfg -> Either JoinError a
runJ j = runReader (runExceptT $ unJ j)

--------------------------------------------------------------------------------
-- $full

-- | Turn a list of KExpr into a CfgToken, throwing errors when encountered.
--
-- NOTE: We start joinConfig with the default JCfg, but joinConfig might locally
-- override settings by things it reads from the config itself.
joinConfigIO :: HasLogFunc e => [KExpr] -> RIO e CfgToken
joinConfigIO es = case runJ joinConfig $ defJCfg es of
  Left  e -> throwM e
  Right c -> pure c

-- | Extract anything matching a particular prism from a list
extract :: Prism' a b -> [a] -> [b]
extract p = mapMaybe (preview p)

data SingletonError
  = None
  | Duplicate

-- | Take the head of a list, or else throw the appropriate error
onlyOne :: [a] -> Either SingletonError a
onlyOne xs = case uncons xs of
  Just (x, []) -> Right x
  Just _       -> Left Duplicate
  Nothing      -> Left None

-- | Take the one and only block matching the prism from the expressions
oneBlock :: Text -> Prism' KExpr a -> J a
oneBlock t l = (view kes <&> (extract l >>> onlyOne)) >>= \case
  Right x        -> pure x
  Left None      -> throwError $ MissingBlock t
  Left Duplicate -> throwError $ DuplicateBlock t

-- | Update the JCfg and then run the entire joining process
joinConfig :: J CfgToken
joinConfig = getOverride >>= \cfg -> local (const cfg) joinConfig'

-- | Join an entire 'CfgToken' from the current list of 'KExpr'.
joinConfig' :: J CfgToken
joinConfig' = do

  es <- view kes

  -- Extract the IO settings
  i  <- getI
  o  <- getO
  ft <- getFT
  al <- getAllow
  ksd <- getKeySeqDelay

  -- Extract the other blocks and join them into a keymap
  let als = extract _KDefAlias es
  let lys = extract _KDefLayer es
  let srcs = extract _KDefSrc es
  (km, fl) <- joinKeymap srcs als lys

  pure $ CfgToken
    { _snk   = o
    , _src   = i
    , _km    = km
    , _fstL  = fl
    , _flt   = ft
    , _allow = al
    , _ksd   = ksd
    }

--------------------------------------------------------------------------------
-- $settings
--
-- TODO: This needs to be seriously refactored: all this code duplication is a
-- sign that something is amiss.

-- | Return a JCfg with all settings from defcfg applied to the env's JCfg
getOverride :: J JCfg
getOverride = do
  -- FIXME: duplicates don't throw errors
  env <- ask
  cfg <- oneBlock "defcfg" _KDefCfg
  let getB = joinButton [] M.empty
  let go e v = case v of
        SCmpSeq b  -> getB b >>= maybe (throwError InvalidComposeKey)
                                       (\b' -> pure $ set cmpKey b' e)
        SImplArnd ia -> pure $ set implArnd ia e
        _ -> pure e
  foldM go env cfg

-- | Turn a 'HasLogFunc'-only RIO into a function from LogFunc to IO
runLF :: HasLogFunc lf => RIO lf a -> lf -> IO a
runLF = flip runRIO

-- | Extract the KeySource-loader from the 'KExpr's
getI :: J (LogFunc -> IO (Acquire KeySource))
getI = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SIToken $ cfg of
    Right i          -> pickInput i
    Left  None       -> throwError $ MissingSetting "input"
    Left  Duplicate  -> throwError $ DuplicateSetting "input"

-- | Extract the KeySource-loader from a 'KExpr's
getO :: J (LogFunc -> IO (Acquire KeySink))
getO = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SOToken $ cfg of
    Right o         -> pickOutput o
    Left  None      -> throwError $ MissingSetting "input"
    Left  Duplicate -> throwError $ DuplicateSetting "input"

-- | Extract the fallthrough setting
getFT :: J Bool
getFT = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SFallThrough $ cfg of
    Right b        -> pure b
    Left None      -> pure False
    Left Duplicate -> throwError $ DuplicateSetting "fallthrough"

-- | Extract the allow-cmd setting
getAllow :: J Bool
getAllow = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SAllowCmd $ cfg of
    Right b        -> pure b
    Left None      -> pure False
    Left Duplicate -> throwError $ DuplicateSetting "allow-cmd"

-- | Extract the cmp-seq-delay setting
getCmpSeqDelay :: J (Maybe Int)
getCmpSeqDelay = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SCmpSeqDelay $ cfg of
    Right 0        -> pure Nothing
    Right b        -> pure (Just b)
    Left None      -> pure Nothing
    Left Duplicate -> throwError $ DuplicateSetting "cmp-seq-delay"

-- | Extract the key-seq-delay setting
getKeySeqDelay :: J (Maybe Int)
getKeySeqDelay = do
  cfg <- oneBlock "defcfg" _KDefCfg
  case onlyOne . extract _SKeySeqDelay $ cfg of
    Right 0        -> pure Nothing
    Right b        -> pure (Just b)
    Left None      -> pure (Just 1)
    Left Duplicate -> throwError $ DuplicateSetting "key-seq-delay"

#ifdef linux_HOST_OS

-- | The Linux correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KDeviceSource f)   = pure $ runLF (deviceSource64 f)
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Linux correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput (KUinputSink t init) = pure $ runLF (uinputSink cfg)
  where cfg = defUinputCfg { _keyboardName = T.unpack t
                           , _postInit     = T.unpack <$> init }
pickOutput (KSendEventSink _)   = throwError $ InvalidOS "SendEventSink"
pickOutput KKextSink            = throwError $ InvalidOS "KextSink"

#endif

#ifdef mingw32_HOST_OS

-- | The Windows correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput KLowLevelHookSource = pure $ runLF llHook
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput (KIOKitSource _)    = throwError $ InvalidOS "IOKitSource"

-- | The Windows correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput (KSendEventSink di) = pure $ runLF (sendEventKeySink di)
pickOutput (KUinputSink _ _)   = throwError $ InvalidOS "UinputSink"
pickOutput KKextSink           = throwError $ InvalidOS "KextSink"

#endif

#ifdef darwin_HOST_OS

-- | The Mac correspondence between IToken and actual code
pickInput :: IToken -> J (LogFunc -> IO (Acquire KeySource))
pickInput (KIOKitSource name) = pure $ runLF (iokitSource (T.unpack <$> name))
pickInput (KDeviceSource _)   = throwError $ InvalidOS "DeviceSource"
pickInput KLowLevelHookSource = throwError $ InvalidOS "LowLevelHookSource"

-- | The Mac correspondence between OToken and actual code
pickOutput :: OToken -> J (LogFunc -> IO (Acquire KeySink))
pickOutput KKextSink            = pure $ runLF kextSink
pickOutput (KUinputSink _ _)    = throwError $ InvalidOS "UinputSink"
pickOutput (KSendEventSink _)   = throwError $ InvalidOS "SendEventSink"

#endif

--------------------------------------------------------------------------------
-- $als

type Aliases = M.HashMap Text DefButton
type LNames  = [Text]

-- | Build up a hashmap of text to button mappings
--
-- Aliases can refer back to other aliases.
joinAliases :: [DefAlias] -> J Aliases
joinAliases als = do
  let als' = concat als
  case find (not . null . NE.tail) $ NE.groupAllWith fst als' of
    Nothing -> pure ()
    Just ((t, _) :| _) -> throwError $ DuplicateAlias t

  M.traverseWithKey =<< go $ M.fromList als'
 where
  go mp t b@(KRef t')
    | t == t' = throwError $ CyclicAlias t
    | otherwise = withDerefAlias mp (go mp t) b
  go _ _ b = pure b

withDerefAlias :: Aliases -> (DefButton -> J a) -> DefButton -> J a
withDerefAlias als f (KRef t) = case M.lookup t als of
  Just b -> f b
  Nothing -> throwError $ MissingAlias t
withDerefAlias _ f b = f b

--------------------------------------------------------------------------------
-- $but

-- | Turn 'Nothing's (caused by joining a KTrans) into the appropriate error.
-- KTrans buttons may only occur in 'DefLayer' definitions.
unnest :: J (Maybe Button) -> J Button
unnest = (maybe (throwError NestedTrans) pure =<<)

fromImplArnd :: DefButton -> DefButton -> ImplArnd -> J DefButton
fromImplArnd _ _ IADisabled        = throwError ImplArndDisabled
fromImplArnd o i IAAround          = pure $ KAround o i
fromImplArnd o i IAAroundOnly      = pure $ KAroundOnly o i
fromImplArnd o i IAAroundWhenAlone = pure $ KAroundWhenAlone o i

-- | Turn a button token into an actual KMonad `Button` value
joinButton :: LNames -> Aliases -> DefButton -> J (Maybe Button)
joinButton ns als =

  -- Define some utility functions
  let ret    = pure . Just
      go     = unnest . joinButton ns als
      jst    = fmap Just
      fi     = fromIntegral
      isps l = traverse go . maybe l ((`intersperse` l) . KPause . fi)
  in \case
    -- Variable dereference
    b@(KRef _) -> withDerefAlias als (joinButton ns als) b

    -- Various simple buttons
    KEmit c -> ret $ emitB c
    KPressOnly c -> ret $ pressOnly c
    KReleaseOnly c -> ret $ releaseOnly c
    KCommand pr mbR -> ret $ cmdButton pr mbR
    KLayerToggle t -> if t `elem` ns
      then ret $ layerToggle t
      else throwError $ MissingLayer t
    KLayerSwitch t -> if t `elem` ns
      then ret $ layerSwitch t
      else throwError $ MissingLayer t
    KLayerAdd t -> if t `elem` ns
      then ret $ layerAdd t
      else throwError $ MissingLayer t
    KLayerRem t -> if t `elem` ns
      then ret $ layerRem t
      else throwError $ MissingLayer t
    KLayerDelay s t -> if t `elem` ns
      then ret $ layerDelay (fi s) t
      else throwError $ MissingLayer t
    KLayerNext t -> if t `elem` ns
      then ret $ layerNext t
      else throwError $ MissingLayer t

    -- Various compound buttons
    KComposeSeq bs     -> do csd <- getCmpSeqDelay
                             c   <- view cmpKey
                             csd' <- for csd $ go . KPause . fi
                             jst $ tapMacro . (c:) . maybe id (:) csd' <$> isps bs csd
    KTapMacro bs mbD   -> jst $ tapMacro           <$> isps bs mbD
    KBeforeAfterNext b a -> jst $ beforeAfterNext <$> go b <*> go a
    KTapMacroRelease bs mbD ->
      jst $ tapMacroRelease           <$> isps bs mbD
    KAround o i        -> jst $ around             <$> go o <*> go i
    KTapNext t h       -> jst $ tapNext            <$> go t <*> go h
    KTapHold s t h     -> jst $ tapHold (fi s)     <$> go t <*> go h
    KTapHoldNext s t h mtb
      -> jst $ tapHoldNext (fi s) <$> go t <*> go h <*> traverse go mtb
    KTapNextRelease t h -> jst $ tapNextRelease    <$> go t <*> go h
    KTapHoldNextRelease ms t h mtb
      -> jst $ tapHoldNextRelease (fi ms) <$> go t <*> go h <*> traverse go mtb
    KTapNextPress t h  -> jst $ tapNextPress       <$> go t <*> go h
    KTapHoldNextPress ms t h mtb
      -> jst $ tapHoldNextPress (fi ms) <$> go t <*> go h <*> traverse go mtb
    KAroundOnly o i    -> jst $ aroundOnly         <$> go o <*> go i
    KAroundWhenAlone o i -> jst $ aroundWhenAlone  <$> go o <*> go i
    KAroundImplicit o i  -> joinButton ns als =<< fromImplArnd o i =<< view implArnd
    KAroundNext b      -> jst $ aroundNext         <$> go b
    KAroundNextSingle b -> jst $ aroundNextSingle <$> go b
    KAroundNextTimeout ms b t -> jst $ aroundNextTimeout (fi ms) <$> go b <*> go t
    KPause ms          -> jst . pure $ onPress (pause ms)
    KMultiTap bs d     -> jst $ multiTap <$> go d <*> mapM f bs
      where f (ms, b) = (fi ms,) <$> go b
    KStepped bs        -> jst $ steppedButton <$> mapM go bs
    KStickyKey s d     -> jst $ stickyKey (fi s) <$> go d

    -- Non-action buttons
    KTrans -> pure Nothing
    KBlock -> ret pass


--------------------------------------------------------------------------------
-- $src

type Sources = M.HashMap (Maybe Text) [Maybe Keycode]

-- | Build up a hashmap of text to source mappings.
joinSources :: Aliases -> [DefSrc] -> J Sources
joinSources als = foldM joiner mempty
  where
   joiner :: Sources -> DefSrc -> J Sources
   joiner sources DefSrc{ _srcName = n, _keycodes = ks }
     | n `M.member` sources = throwError $ DuplicateSource n
     | otherwise            = do
        ks' <- for ks $ withDerefAlias als joinKeycode
        let dups = NE.head <$> duplicatesWith id (toList =<< ks')
        unless (null dups) $ throwError $ DuplicateKeyInSource n dups
        pure $ M.insert n ks' sources
    where
     joinKeycode (KEmit k) = pure $ Just k
     joinKeycode KBlock    = pure Nothing
     joinKeycode b         = throwError $ NotAKeycode b

--------------------------------------------------------------------------------
-- $kmap

-- | Join the defsrc, defalias, and deflayer layers into a Keymap of buttons and
-- the name signifying the initial layer to load.
joinKeymap :: [DefSrc] -> [DefAlias] -> [DefLayer] -> J (LMap Button, LayerTag)
joinKeymap []   _   _   = throwError $ MissingBlock "defsrc"
joinKeymap _    _   []  = throwError $ MissingBlock "deflayer"
joinKeymap srcs als lys = do
  let f acc x = if x `elem` acc then throwError $ DuplicateLayer x else pure (x:acc)
  nms   <- foldM f [] $ map _layerName lys     -- Extract all names
  als'  <- joinAliases als                     -- Join aliases into 1 hashmap
  srcs' <- joinSources als' srcs               -- Join all sources into 1 hashmap
  lys'  <- mapM (joinLayer als' nms srcs') lys -- Join all layers
  -- Return the layerstack and the name of the first layer
  pure (L.mkLayerStack lys', _layerName . fromJust . headMaybe $ lys)

-- | Check and join 1 deflayer.
joinLayer ::
     Aliases                       -- ^ Mapping of names to buttons
  -> LNames                        -- ^ List of valid layer names
  -> Sources                       -- ^ Mapping of names to source layer
  -> DefLayer                      -- ^ The layer token to join
  -> J (Text, [(Keycode, Button)]) -- ^ The resulting tuple
joinLayer als ns srcs l@(DefLayer n settings) = do
  let bs = settings ^.. each . _LButton
  assocSrc <- getAssocSrc l
  implAround <- getImplAround l

  src <- case M.lookup assocSrc srcs of
    Just src -> pure src
    Nothing  -> throwError $ MissingSource assocSrc
  -- Ensure length-match between src and buttons
  when (length bs /= length src) $
    throwError $ LengthMismatch n (length bs) (length src)

  -- Join each button and add it (filtering out KTrans)
  let
    f acc (Nothing, b) = joinButton ns als b $> acc
    f acc (Just kc, b) = joinButton ns als b >>= \case
      Nothing -> pure acc
      Just b' -> pure $ (kc, b') : acc
  maybe id (local . set implArnd) implAround $
    (n,) <$> foldM f [] (zip src bs)

getAssocSrc :: DefLayer -> J (Maybe Text)
getAssocSrc (DefLayer n settings) = case onlyOne (settings ^.. each . _LSrcName) of
  Right x        -> pure $ Just x
  Left None      -> pure Nothing
  Left Duplicate -> throwError $ DuplicateLayerSetting n "source"

getImplAround :: DefLayer -> J (Maybe ImplArnd)
getImplAround (DefLayer n settings) = case onlyOne (settings ^.. each . _LImplArnd) of
  Right x        -> pure $ Just x
  Left None      -> pure Nothing
  Left Duplicate -> throwError $ DuplicateLayerSetting n "implicit-around"

--------------------------------------------------------------------------------
-- $test

-- fname :: String
-- fname = "/home/david/prj/hask/kmonad/doc/example.kbd"

-- test :: IO (J DefCfg)
-- test = runRIO () . fmap joinConfig $ loadTokens fname
