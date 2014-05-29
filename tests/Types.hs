module Types where

data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  -- , lat      :: Float
  -- , lng      :: Float
  , location :: Coords
  } deriving (Eq, Show)

data Gender = Male | Female
  deriving (Eq, Show)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show)
