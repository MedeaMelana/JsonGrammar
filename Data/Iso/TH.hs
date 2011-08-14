{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Iso.TH (deriveIsos) where

import Data.Iso.Core
import Language.Haskell.TH
import Control.Applicative
import Control.Monad
import Control.Arrow


-- | Derive partial isomorphisms for a given datatype. The resulting
-- expression is a tuple with one isomorphism element for each constructor in
-- the datatype.
-- 
-- For example:
-- 
-- > nothing :: Iso t (Maybe a :- t)
-- > just    :: Iso (a :- t) (Maybe a :- t)
-- > (nothing, just) = $(deriveIsos ''Maybe)
-- 
-- Deriving isomorphisms this way requires @-XNoMonoPatBinds@.
deriveIsos :: Name -> Q Exp
deriveIsos name = do
  info <- reify name
  routers <-
    case info of
      TyConI (DataD _ _ _ cons _)   ->
        mapM (deriveIso (length cons /= 1)) cons
      TyConI (NewtypeD _ _ _ con _) ->
        (:[]) <$> deriveIso False con
      _ ->
        fail $ show name ++ " is not a datatype."
  return (TupE routers)


deriveIso :: Bool -> Con -> Q Exp
deriveIso matchWildcard con =
  case con of
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    _ -> fail $ "Unsupported constructor " ++ show (conName con)
  where
    go name tys = do
      iso <- [| Iso |]
      isoCon <- deriveConstructor name tys
      isoDes <- deriveDestructor matchWildcard name tys
      return $ iso `AppE` isoCon `AppE` isoDes


deriveConstructor :: Name -> [Type] -> Q Exp
deriveConstructor name tys = do
  -- Introduce some names
  t          <- newName "t"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  ret       <- [| return |]
  ConE cons <- [| (:-) |]
  kleisli   <- [| Kleisli |]

  let pat = foldr (\f fs -> ConP cons [VarP f, fs]) (VarP t) fieldNames
  let applyCon = foldl (\f x -> f `AppE` VarE x) (ConE name) fieldNames
  -- applyCon <- [| undefined |]
  let body = ret `AppE` (ConE cons `AppE` applyCon `AppE` VarE t)

  return $ kleisli `AppE` LamE [pat] body


deriveDestructor :: Bool -> Name -> [Type] -> Q Exp
deriveDestructor matchWildcard name tys = do
  -- Introduce some names
  x          <- newName "x"
  r          <- newName "r"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  ret        <- [| return |]
  ConE cons  <- [| (:-) |]
  nothing    <- [| Nothing |]
  kleisli    <- [| Kleisli |]

  let conPat   = ConP name (map VarP fieldNames)
  let okBody   = ret `AppE`
                  foldr
                    (\h t -> ConE cons `AppE` VarE h `AppE` t)
                    (VarE r)
                    fieldNames
  let okCase   = Match (ConP cons [conPat, VarP r]) (NormalB okBody) []
  let failCase = Match WildP (NormalB nothing) []
  let allCases =
        if matchWildcard
              then [okCase, failCase]
              else [okCase]

  return $ kleisli `AppE` LamE [VarP x] (CaseE (VarE x) allCases)


-- Retrieve the name of a constructor.
conName :: Con -> Name
conName con =
  case con of
    NormalC name _  -> name
    RecC name _     -> name
    InfixC _ name _ -> name
    ForallC _ _ con' -> conName con'