module CoreIR where

import Data.List (intercalate)
import GHC
import GHC.Core
import GHC.Driver.Ppr
import GHC.Plugins (Literal, Var (varName), litValue, occName, occNameString)
import Text.Printf (printf)

indent :: String -> String
indent = unlines . map ("  " ++) . lines

varToString :: Var -> String
varToString var = occNameString $ occName $ varName var

literalToInt :: Literal -> Int
literalToInt literal = fromIntegral $ litValue literal

prettyCoreBind :: DynFlags -> CoreBind -> String
prettyCoreBind dflags bind = case bind of
  NonRec b e -> printf "NonRec(%s =\n%s)\n" (showPpr dflags b) (indent (prettyCoreExpr dflags e))
  Rec binds -> printf "Rec({\n%s})\n" (indent (intercalate ",\n" (map (prettyBind dflags) binds)))

prettyBind :: DynFlags -> (Var, CoreExpr) -> String
prettyBind dflags (b, e) = printf "(%s = \n%s)" (showPpr dflags b) (indent (prettyCoreExpr dflags e))

prettyCoreProgram :: DynFlags -> CoreProgram -> String
prettyCoreProgram dflags = intercalate "\n" . map (prettyCoreBind dflags)

prettyCoreExpr :: DynFlags -> CoreExpr -> String
prettyCoreExpr dflags expr = case expr of
  Var i -> printf "Var(%s)" (varToString i)
  Lit l -> printf "Lit(%s)" (showPpr dflags l)
  App e a -> printf "App(%s *\n%s)" (prettyCoreExpr dflags e) (prettyCoreExpr dflags a)
  Lam b e -> printf "Lam(%s ->\n%s)" (varToString b) (prettyCoreExpr dflags e)
  Type t -> printf "Type(%s)" (showPpr dflags t)
  Let bind e -> printf "Let(\n%s in\n%s)" (indent (prettyCoreBind dflags bind)) (indent (prettyCoreExpr dflags e))
  Case e b _ alts -> printf "Case(%s of %s {\n%s})" (prettyCoreExpr dflags e) (showPpr dflags b) (indent (prettyCoreAltList dflags alts))
  Cast e co -> printf "Cast(%s by %s)" (prettyCoreExpr dflags e) (showPpr dflags co)
  Tick t e -> printf "Tick(%s: %s)" (showPpr dflags t) (prettyCoreExpr dflags e)
  Coercion co -> printf "Coercion(%s)" (showPpr dflags co)

prettyCoreAlt :: DynFlags -> CoreAlt -> String
prettyCoreAlt dflags (Alt con bl e) = printf "Alt(%s: {%s} =\n%s)" (prettyCoreAltCon dflags con) (prettyCoreBndrList dflags bl) (indent (prettyCoreExpr dflags e))

prettyCoreAltList :: DynFlags -> [CoreAlt] -> String
prettyCoreAltList dflags = intercalate ",\n" . map (prettyCoreAlt dflags)

prettyCoreBndrList :: DynFlags -> [Var] -> String
prettyCoreBndrList dflags = intercalate ", " . map (showPpr dflags)

prettyCoreAltCon :: DynFlags -> AltCon -> String
prettyCoreAltCon dflags altCons = case altCons of
  DataAlt d -> printf "DataAlt(%s)" (showPpr dflags d)
  LitAlt l -> printf "LitAlt(%s)" (showPpr dflags l)
  DEFAULT -> "DEFAULT"
