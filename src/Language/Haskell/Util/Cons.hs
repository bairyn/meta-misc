{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.Util.Cons
	( decons
	) where

import Data.List
import Debug.Trace.LocationTH
import Language.Haskell.TH
import Text.Printf

-- | Generate a deconstructor that takes as input a value and returns 'Maybe'
-- the deconstructed value.
--
-- This functions as a Data Constructor inverter.  If the number of
-- parameters of a constructor is not equal to 1, 'Maybe' a tuple is returned.--  The result of this function is defined only when 
--
-- Example:
--
-- @
-- $(decons 'Right) (Right "foo") :: Maybe String
-- $(decons '(:)) "bar" :: Maybe (Char, String)
-- @
decons :: Name -> Q Exp
decons n_cons = do
    n_x <- newName "x"
    info <- reify n_cons
    let typeOfCons =
            case info of
                (DataConI _ t _ _)  ->
                    t
                _                   ->
                    $failure $ printf "the referent of the name '%s' is invalid; it should be a data constructor ('DataConI')" (nameBase n_cons)
    --flip seq (return ()) $ typeOfCons  -- TODO: jj
    let numParams :: Type -> Integer
        numParams (ForallT _ _ t) = numParams t
        numParams (VarT _)        = 0
        numParams (ConT _)        = 0
        numParams (TupleT _)      = 0
        numParams (ArrowT)        = 0
        numParams (ListT)         = 0
        numParams (AppT ArrowT _) = 1
        numParams (AppT a b)      = numParams a + numParams b
        numParams (SigT t _)      = numParams t
    names <- sequence . flip genericReplicate (newName "a") $ numParams typeOfCons
    return $
        LamE [VarP n_x] $                                                  -- \x ->
            CaseE (VarE n_x) $                                             --     case x of
                [ flip (Match (ConP n_cons $ map VarP names)) [] $         --         (Cons a b …) ->
                    NormalB . AppE (ConE 'Just) . TupE . map VarE $ names  --             Just (a, b, …)
                , flip (Match WildP) [] $                                  --         _            ->
                    NormalB $ ConE 'Nothing                                --             Nothing
                ]
