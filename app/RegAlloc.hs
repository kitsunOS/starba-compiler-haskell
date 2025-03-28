module RegAlloc where
import qualified IR
import qualified IRReg
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Register
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

data LiveSets = LiveSets {
  liveInstrs :: [Set.Set IR.RegName],
  liveOuts :: Set.Set IR.RegName,
  liveIns :: Set.Set IR.RegName
} deriving (Show, Eq)

type InterferenceGraph = Map.Map IR.RegName (Set.Set IR.RegName)

data Allocation a = Allocation {
  allocatedRegisters :: Map.Map IR.RegName a,
  spiltRegisters :: [IR.RegName]
} deriving (Show, Eq)

emptyLiveSets :: LiveSets
emptyLiveSets = LiveSets [] Set.empty Set.empty

allocateRegisters :: (Ord a, Register.Register a) => [IR.Block] -> Set.Set a -> Allocation a
allocateRegisters blocks =
  let
    liveSets' = map (liveSets . IR.blockInstructions) blocks
    interferences' = foldl (\m (b, l) -> Map.union m (interferences b l)) Map.empty (zip blocks liveSets')
  in
    colors interferences'

liveSets :: [IR.Instruction] -> LiveSets
liveSets instructions = snd $ foldl liveSet (Set.empty, emptyLiveSets) (reverse instructions)
  where
    liveSet :: (Set.Set IR.RegName, LiveSets) -> IR.Instruction -> (Set.Set IR.RegName, LiveSets)
    liveSet (live, liveSets') instr =
      let
        defs' = Set.fromList (IRReg.defs instr)
        uses' = Set.fromList (IRReg.uses instr)
        live' = (live Set.\\ defs') `Set.union` uses'
        instrs = live' : liveInstrs liveSets'
        liveOuts' = Set.union (liveOuts liveSets') defs'
        liveIns' = Set.union (liveIns liveSets') uses' Set.\\ defs'
      in
        (live', LiveSets {
          liveInstrs = instrs,
          liveOuts = liveOuts',
          liveIns = liveIns'})

-- TODO: Using liveIns and liveOuts, back-insert interfering registers regarding other blocks

interferences :: IR.Block -> LiveSets -> InterferenceGraph
interferences block liveSets = foldl collapseMap Map.empty (zip (IR.blockInstructions block) (liveInstrs liveSets))
  where
    collapseMap :: InterferenceGraph -> (IR.Instruction, Set.Set IR.RegName) -> InterferenceGraph
    collapseMap map' (instr, live) =
      insertDefs map' (IRReg.uses instr) live
    insertDefs :: InterferenceGraph -> [IR.RegName] -> Set.Set IR.RegName -> InterferenceGraph
    insertDefs map' uses live = foldl (\m u -> insertDef (live Set.\\ Set.singleton u) m u) map' uses
    insertDef :: Set.Set IR.RegName -> InterferenceGraph -> IR.RegName -> InterferenceGraph
    insertDef live map' use =
      Map.insert use (Set.union live (Map.findWithDefault Set.empty use g)) g
      where g = foldl (\m reg -> Map.insert reg (Set.insert use (Map.findWithDefault Set.empty reg m)) m) map' live

colors :: (Ord a, Register.Register a) => InterferenceGraph -> Set.Set a -> Allocation a
colors graph allRegs = colorWithSpills graph allRegs (Allocation Map.empty [])

colorWithSpills :: (Ord a, Register.Register a) => InterferenceGraph -> Set.Set a -> Allocation a -> Allocation a
colorWithSpills graph allRegs alloc = case colorNext graph allRegs alloc of
  Left _ -> do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let graph' = graphWithout maxReg graph
    let alloc' = Allocation (allocatedRegisters alloc) (maxReg : spiltRegisters alloc)
    colorWithSpills graph' allRegs alloc'
  Right alloc' -> alloc'

colorNext :: (Ord a, Register.Register a) => InterferenceGraph -> Set.Set a -> Allocation a -> Either () (Allocation a)
colorNext graph allRegs alloc
  | Map.null graph = Right alloc
  | otherwise = do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let conflicts = graph Map.! maxReg
    let conflictsPhys = foldl (\s reg -> maybe s (`Set.insert` s) (Map.lookup reg (allocatedRegisters alloc))) Set.empty conflicts
    let avail = allRegs Set.\\ conflictsPhys
    if Set.null avail then Left () else do
      let reg32 = head $ Set.toList avail
      let graph' = Map.delete maxReg graph
      let alloc' = Allocation (Map.insert maxReg reg32 (allocatedRegisters alloc)) (spiltRegisters alloc)
      colorNext graph' allRegs alloc'

graphWithout :: IR.RegName -> InterferenceGraph -> InterferenceGraph
graphWithout reg graph = Map.delete reg $ Map.map (Set.delete reg) graph

-- Lower is better
-- TODO: Maybe account for how many times a regname is used?
weightRegName :: Set.Set IR.RegName -> Int
weightRegName = Set.size