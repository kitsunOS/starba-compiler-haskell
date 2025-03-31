module RegAlloc where
import qualified IR
import qualified IRReg
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Register
import Data.Foldable (maximumBy, Foldable (fold))
import Data.Ord (comparing)
import qualified X86.X86Reg as X86Reg

data LiveSets a = LiveSets {
  liveInstrs :: [Set.Set (IR.RegEntry a)],
  liveOuts :: Set.Set (IR.RegEntry a),
  liveIns :: Set.Set (IR.RegEntry a)
} deriving (Show, Eq)

data Allocation a = Allocation {
  allocatedRegisters :: Map.Map IR.RegName a,
  spiltRegisters :: [IR.RegName]
} deriving (Show, Eq)

newtype RegAllocContext a = RegAllocContext {
  intLive :: IR.Instruction -> [IR.RegEntry a]
}

type InterferenceGraph a = Map.Map IR.RegName (Set.Set (IR.RegEntry a))

emptyLiveSets :: LiveSets a
emptyLiveSets = LiveSets [] Set.empty Set.empty

allocateRegisters :: (Ord a, Register.Register a) => RegAllocContext a -> [IR.Block] -> Set.Set a -> Allocation a
allocateRegisters ctx blocks =
  let
    liveSets' = map (liveSets ctx . IR.blockInstructions) blocks
    interferences' = foldl (\m (b, l) -> Map.union m (interferences b l)) Map.empty (zip blocks liveSets')
  in
    colors interferences'

liveSets :: (Ord a, Register.Register a) => RegAllocContext a -> [IR.Instruction] -> LiveSets a
liveSets ctx instructions = snd $ foldl (liveSet ctx) (Set.empty, emptyLiveSets) (reverse instructions)
  where
    liveSet :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> (Set.Set (IR.RegEntry a0), LiveSets a0) -> IR.Instruction -> (Set.Set (IR.RegEntry a0), LiveSets a0)
    liveSet ctx (live, liveSets') instr =
      let
        defs' = Set.fromList (map IR.Virtual (IRReg.defs instr))
        uses' = Set.fromList (map IR.Virtual (IRReg.uses instr))
        live' = (live Set.\\ defs') `Set.union` uses'
        localLive = live' `Set.union` Set.fromList (intLive ctx instr)
        instrs = (live' `Set.union` localLive) : liveInstrs liveSets'
        liveOuts' = Set.union (liveOuts liveSets') defs'
        liveIns' = Set.union (liveIns liveSets') uses' Set.\\ defs'
      in
        (live', LiveSets {
          liveInstrs = instrs,
          liveOuts = liveOuts',
          liveIns = liveIns'})

-- TODO: Using liveIns and liveOuts, back-insert interfering registers regarding other blocks

interferences :: (Ord a, Register.Register a) => IR.Block -> LiveSets a -> InterferenceGraph a
interferences block liveSets = foldl collapseMap Map.empty (zip (IR.blockInstructions block) (liveInstrs liveSets))
  where
    collapseMap :: (Ord a0, Register.Register a0) => InterferenceGraph a0 -> (IR.Instruction, Set.Set (IR.RegEntry a0)) -> InterferenceGraph a0
    collapseMap map' (instr, live) = insertDefs map' (IRReg.uses instr) live
    insertDefs :: (Ord a0, Register.Register a0) => InterferenceGraph a0 -> [IR.RegName] -> Set.Set (IR.RegEntry a0) -> InterferenceGraph a0
    insertDefs map' uses live = foldl (\m u -> insertDef (live Set.\\ Set.singleton (IR.Virtual u)) m u) map' uses
    insertDef :: (Ord a0, Register.Register a0) => Set.Set (IR.RegEntry a0) -> InterferenceGraph a0 -> IR.RegName -> InterferenceGraph a0
    insertDef live map' use =
      Map.insert use (Set.union live (Map.findWithDefault Set.empty use g)) g
      where g = foldl (\m re -> case re of
              IR.Virtual regName -> Map.insert regName (Set.insert (IR.Virtual use) (Map.findWithDefault Set.empty regName m)) m
              IR.Physical _ -> m) map' live

colors :: (Ord a, Register.Register a) => InterferenceGraph a -> Set.Set a -> Allocation a
colors graph allRegs = colorWithSpills graph allRegs (Allocation Map.empty [])

colorWithSpills :: (Ord a, Register.Register a) => InterferenceGraph a -> Set.Set a -> Allocation a -> Allocation a
colorWithSpills graph allRegs alloc = case colorNext graph allRegs alloc of
  Left _ -> do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let graph' = graphWithout maxReg graph
    let alloc' = Allocation (allocatedRegisters alloc) (maxReg : spiltRegisters alloc)
    colorWithSpills graph' allRegs alloc'
  Right alloc' -> alloc'

colorNext :: (Ord a, Register.Register a) => InterferenceGraph a -> Set.Set a -> Allocation a -> Either () (Allocation a)
colorNext graph allRegs alloc
  | Map.null graph = Right alloc
  | otherwise = do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let conflicts = graph Map.! maxReg
    let conflictsPhys = foldl (\s reg -> case reg of
          IR.Virtual regName -> maybe s (`Set.insert` s) (Map.lookup regName (allocatedRegisters alloc))
          IR.Physical reg' -> Set.insert reg' s) Set.empty conflicts
    let avail = allRegs Set.\\ conflictsPhys
    if Set.null avail then Left () else do
      let reg32 = head $ Set.toList avail
      let graph' = Map.delete maxReg graph
      let alloc' = Allocation (Map.insert maxReg reg32 (allocatedRegisters alloc)) (spiltRegisters alloc)
      colorNext graph' allRegs alloc'

graphWithout :: (Ord a, Register.Register a) => IR.RegName -> InterferenceGraph a -> InterferenceGraph a
graphWithout reg graph = Map.delete reg $ Map.map (Set.delete (IR.Virtual reg)) graph

-- Lower is better
-- TODO: Maybe account for how many times a regname is used?
weightRegName :: Set.Set (IR.RegEntry a) -> Int
weightRegName = Set.size