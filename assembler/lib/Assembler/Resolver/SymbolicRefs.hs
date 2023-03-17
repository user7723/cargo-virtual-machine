module Assembler.Resolver.SymbolicRefs where

import Assembler.Resolver.ProgramCodeDeps

import Assembler.IR.Aliases
import Assembler.IR.Operator
import Assembler.IR.StaticMemory
import Assembler.IR.FunctionDef
import Assembler.IR.QLabel
import Assembler.IR.RawFormat

import Data.Word
import Data.List

import qualified Data.Set as S
import qualified Data.Map as M

flatData :: SegData -> [InitDirective]
flatData (SegData m _) = M.elems $ M.fromList $ M.elems m

flatBss :: SegBss -> [AllocDirective]
flatBss (SegBss m _) = M.elems $ M.fromList $ M.elems m

functionDefs :: SegText -> [FunctionDef]
functionDefs (SegText m _) = M.elems $ M.fromList $ M.elems m

functionAddrs :: SegText -> [(QLabel, Word64)]
functionAddrs (SegText m _) = sortOn snd $ map (fmap fst) $ M.toList m

resolveSymbolicRefs :: Segs -> ProgramText
resolveSymbolicRefs s@Segs{..} =
  let
    txt = concatMap (resolveSRFunctionDef s) (functionDefs sectionText)
    dat = flatData sectionData
    bss = flatBss sectionBss
    txtSymbols = functionAddrs sectionText
  in ProgramText txt dat bss txtSymbols

resolveSRFunctionDef :: Segs -> FunctionDef -> [Operator]
resolveSRFunctionDef segs FunctionDef{..} =
  let fb' = (S.foldr
              (\idx fb -> M.adjust (resolveSROperator segs) idx fb)
              functionInsts
              functionUnresolved ):: Word64 :-> Operator
  in M.elems fb'

resolveSROperator :: Segs -> Operator -> Operator
resolveSROperator
  Segs{..}
  (Unary inst
    (OperandAddress
      (Symbolic
        (ql@QLabel{..}))))
  = case labelRelative of
      Just lbl ->
        let (base, FunctionDef{..}) = sT M.! (ql { labelRelative = Nothing })
            offset = functionLabels M.! lbl
        in Unary inst (OperandAddress (Numeric Text (base + offset)))
      Nothing -> case labelSegType of
        Data  -> mkOp inst Data (unsafeLookup ql sD)
        Bss   -> mkOp inst Bss  (unsafeLookup ql sB)
        Text  -> mkOp inst Text (unsafeLookup ql sT)
        Local -> error "you fucked up with locals"
  where
    unsafeLookup q s = maybe err fst (M.lookup q s)
    mkOp i s a = Unary i (OperandAddress (Numeric s a))
    err = error $ "Refactor your garbage! You have an undefined reference to: " ++ show ql
    (SegText sT _) = sectionText
    (SegData sD _) = sectionData
    (SegBss sB _ ) = sectionBss
resolveSROperator _ n = n
