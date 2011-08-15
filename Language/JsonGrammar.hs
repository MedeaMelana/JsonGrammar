{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JsonGrammar (
  -- * Constructing JSON grammars
  Grammar,
  liftAeson, option, greedyOption, list, elementBy, array,
  propBy, rawFixedProp, rest, ignoreRest, object,
  
  -- * Type-directed conversion
  Json(..), fromJson, fromJsonSource, toJson, toJsonSource,
  litJson, prop, fixedProp, element
  
  ) where

import Prelude hiding (id, (.), head, maybe, either)

import Data.Aeson hiding (object)
import Data.Aeson.Types (parse)
import Data.Attoparsec (parseOnly)
import Data.Attoparsec.Number
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Int
import Data.IntSet
import Data.Iso hiding (option)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Time.Clock
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Stream as VS
import Data.Word

import Control.Applicative hiding (many)
import Control.Arrow
import Control.Category
import Control.Monad

-- | Specialized result monads for JSON grammars.
type Grammar = Iso FromJsonResult Maybe

data FromJsonResult a
  = ResultSuccess a
  | ResultErrors [FromJsonError]
  -- empty list means: grammar was empty
  -- multiple errors means: only one of them needs to be fixed for progress

data FromJsonError
  = ExpectedProperty Text
  | UnexpectedProperties [Text]
  | ExpectedArray
  | ExpectedEndOfArray
  | ExpectedObject
  | ExpectedLiteral Value
  | AesonError String

instance Functor FromJsonResult where
  fmap = liftM

instance Applicative FromJsonResult where
  pure = return
  (<*>) = ap

instance Monad FromJsonResult where
  return = ResultSuccess
  res >>= f =
    case res of
      ResultSuccess x   -> f x
      ResultErrors errs -> ResultErrors errs

instance Alternative FromJsonResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus FromJsonResult where
  mzero                                      = ResultErrors []
  ResultSuccess x    `mplus` _               = ResultSuccess x
  _                  `mplus` ResultSuccess y = ResultSuccess y
  ResultErrors xs    `mplus` ResultErrors ys = ResultErrors (xs ++ ys)

aeObject :: (Monad m, MonadPlus n, Functor m, Functor n) =>
  Iso m n (Object :- t) (Value :- t)
aeObject = stack $ Iso
  (arr Object)
  (Kleisli $ \v -> case v of Object o -> return o; _ -> mzero)

aeArray  :: (Monad m, MonadPlus n, Functor m, Functor n) =>
  Iso m n (Array  :- t) (Value :- t)
aeArray = stack $ Iso
  (arr Array)
  (Kleisli $ \v -> case v of Array o -> return o; _ -> mzero)

aeNull   :: (Monad m, MonadPlus n, Functor m, Functor n) =>
  Iso m n t (Value :- t)
aeNull = Iso
  (arr (Null :-))
  (Kleisli $ \v -> case v of Null :- t -> return t; _ -> mzero)

-- | Convert any Aeson-enabled type to a grammar.
liftAeson :: (FromJSON a, ToJSON a) => Grammar (Value :- t) (a :- t)
liftAeson = stack (Iso from to)
  where
    from = Kleisli $ \value -> case parse parseJSON value of
            Error message -> ResultErrors [AesonError message]
            Success x     -> ResultSuccess x
    to   = arr toJSON

-- | Introduce 'Null' as possible value. First gives the argument grammar a
-- chance, only yielding 'Null' or 'Nothing' if the argument grammar fails to
-- handle the input.
option :: Grammar (Value :- t) (a :- t) -> Grammar (Value :- t) (Maybe a :- t)
option g = just . g <> nothing . inverse aeNull

-- | Introduce 'Null' as possible (greedy) value. Always converts 'Nothing' to
-- 'Null' and vice versa, even if the argument grammar knows how to handle
-- these values.
greedyOption :: Grammar (Value :- t) (a :- t) ->
  Grammar (Value :- t) (Maybe a :- t)
greedyOption g = nothing . inverse aeNull <> just . g

-- | Convert between a JSON array and Haskell list of arbitrary lengts. The
-- elements are converted using the argument grammar.
list :: Grammar (Value :- t) (a :- t) -> Grammar (Value :- t) ([a] :- t)
list g = duck nil >>> array (many single)
  where
    -- With ScopedTypeVariables:
    -- single :: Grammar ([Value] :- [a] :- t) ([Value] :- [a] :- t)
    single = swap                -- [a] :- [Value] :- t
         >>> duck (elementBy g)  -- [a] :- [Value] :- a :- t
         >>> swap                -- [Value] :- [a] :- a :- t
         >>> duck swap           -- [Value] :- a :- [a] :- t
         >>> duck cons           -- [Value] :- [a] :- t

-- | Wrap a bunch of elements in a JSON array. For example, to match an array
-- of exactly length two:
--
-- > array (element . element)
--
-- Or to match an empty array:
--
-- > array id
array :: Grammar ([Value] :- t1) ([Value] :- t2) -> Grammar (Value :- t1) t2
array els = inverse aeArray    -- Vector Value :- t1
        >>> vectorReverseList  -- [Value] :- t1
        >>> els                -- [Value] :- t2
        >>> inverse nil        -- t2

-- | Describe a single array element with the given grammar.
elementBy :: Grammar (Value :- t1) t2 ->
  Grammar ([Value] :- t1) ([Value] :- t2)
elementBy g = inverse cons  -- Value   :- [Value] :- t
          >>> swap          -- [Value] :- Value :- t
          >>> duck g        -- [Value] :- a :- t

vectorReverseList :: Grammar (V.Vector a :- t) ([a] :- t)
vectorReverseList = stack (Iso f g)
  where
    f = arr (VS.toList    . VG.streamR)
    g = arr (VG.unstreamR . VS.fromList)


-- | Describe a property with the given name and value grammar.
propBy :: Grammar (Value :- t) (a :- t) -> String ->
  Grammar (Object :- t) (Object :- a :- t)
propBy g name = duck g . rawProp name

rawProp :: String -> Grammar (Object :- t) (Object :- Value :- t)
rawProp name = Iso from to
  where
    textName = fromString name
    from = Kleisli $ \(o :- r) -> do
      case M.lookup textName o of
        Nothing    -> ResultErrors [ExpectedProperty textName]
        Just value -> return (M.delete textName o :- value :- r)
    to = Kleisli $ \(o :- value :- r) -> do
      guard (M.notMember textName o)  -- todo: throw PropertyAlreadyExists
      return (M.insert textName value o :- r)

-- | Expect a specific key/value pair.
rawFixedProp :: String -> Value -> Grammar (Object :- t) (Object :- t)
rawFixedProp name value = stack (Iso from to)
  where
    textName = fromString name
    from = Kleisli $ \o -> do
      case M.lookup textName o of
        Nothing -> ResultErrors [ExpectedProperty textName]
        Just value' -> do
          if value' == value
            then return (M.delete textName o)
            else ResultErrors [ExpectedLiteral value]
    to = Kleisli $ \o -> do
      guard (M.notMember textName o)
      return (M.insert textName value o)

-- | Collect all properties left in an object.
rest :: Grammar (Object :- t) (Object :- M.Map Text Value :- t)
rest = lit M.empty

-- | Match and discard all properties left in the object. When converting back
-- to JSON, produces no properties.
ignoreRest :: Grammar (Object :- t) (Object :- t)
ignoreRest = lit M.empty . inverse (ignoreWithDefault M.empty)

-- | Wrap an exhaustive bunch of properties in an object. Typical usage:
-- 
-- > object (prop "key1" . prop "key2")
object :: Grammar (Object :- t1) (Object :- t2) -> Grammar (Value :- t1) t2
object props = inverse aeObject >>> props >>> inverseLit M.empty


-- Type-directed conversion

-- | Convert values of a type to and from JSON.
class Json a where
  grammar :: Grammar (Value :- t) (a :- t)

instance Json a => Json [a] where
  grammar = list grammar

instance Json a => Json (Maybe a) where
  grammar = option grammar

instance (Json a, Json b) => Json (Either a b) where
  grammar = either grammar grammar


instance Json Bool            where grammar = liftAeson
instance Json Char            where grammar = liftAeson
instance Json Double          where grammar = liftAeson
instance Json Float           where grammar = liftAeson
instance Json Int             where grammar = liftAeson
instance Json Int8            where grammar = liftAeson
instance Json Int16           where grammar = liftAeson
instance Json Int32           where grammar = liftAeson
instance Json Int64           where grammar = liftAeson
instance Json Integer         where grammar = liftAeson
instance Json Word            where grammar = liftAeson
instance Json Word8           where grammar = liftAeson
instance Json Word16          where grammar = liftAeson
instance Json Word32          where grammar = liftAeson
instance Json Word64          where grammar = liftAeson
instance Json ()              where grammar = liftAeson
instance Json ByteString      where grammar = liftAeson
instance Json Lazy.ByteString where grammar = liftAeson
instance Json Number          where grammar = liftAeson
instance Json Text            where grammar = liftAeson
instance Json Lazy.Text       where grammar = liftAeson
instance Json IntSet          where grammar = liftAeson
instance Json UTCTime         where grammar = liftAeson
instance Json DotNetTime      where grammar = liftAeson
instance Json Value           where grammar = id
instance Json [Char]          where grammar = liftAeson

unsafeToJson :: Json a => String -> a -> Value
unsafeToJson context value =
    fromMaybe err (convert (inverse (unstack grammar)) value)
  where
    err = error (context ++
            ": could not convert Haskell value to JSON value")

-- | Convert from JSON.
fromJson :: Json a => Value -> FromJsonResult a
fromJson = convert (unstack grammar)

fromJsonSource :: Json a => ByteString -> FromJsonResult a
fromJsonSource source =
  case parseOnly json source of
    Left message -> ResultErrors [AesonError message]
    Right value  -> fromJson value

-- | Convert to JSON.
toJson :: Json a => a -> Maybe Value
toJson = convert (inverse (unstack grammar))

toJsonSource :: Json a => a -> Maybe Lazy.ByteString
toJsonSource = fmap encode . toJson

-- | Expect/produce a specific JSON 'Value'.
litJson :: Json a => a -> Grammar (Value :- t) t
litJson = inverseLit . unsafeToJson "litJson"

-- | Describe a property whose value grammar is described by a 'Json'
-- instance.
prop :: Json a => String -> Grammar (Object :- t) (Object :- a :- t)
prop = propBy grammar

-- | Expect a specific key/value pair.
fixedProp :: Json a => String -> a -> Grammar (Object :- t) (Object :- t)
fixedProp name value = rawFixedProp name (unsafeToJson "fixedProp" value)

-- | Describe a single array element whose grammar is given by a 'Json'
-- instance.
element :: Json a => Grammar ([Value] :- t) ([Value] :- a :- t)
element = elementBy grammar
