module RegAlloc where
import qualified IR
import qualified IRInstrAnalysis as IRIA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Register
import Data.Foldable (maximumBy, Foldable (fold))
import Data.Ord (comparing)
import qualified X86.X86Reg as X86Reg
import qualified Debug.Trace as Debug

data BlockUseDefs a = BlockUseDefs {
  uses :: Set.Set (IR.RegEntry a),
  defs :: Set.Set (IR.RegEntry a),
  successors :: Set.Set IR.LabelRef
} deriving (Show, Eq)

data BlockInOut a = BlockInOut {
  liveIn :: Set.Set (IR.RegEntry a),
  liveOut :: Set.Set (IR.RegEntry a)
} deriving (Show, Eq)

newtype LiveSets a = LiveSets {
  liveInstrs :: [Set.Set (IR.RegEntry a)]
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
emptyLiveSets = LiveSets []

emptyUseDefs :: BlockUseDefs a
emptyUseDefs = BlockUseDefs Set.empty Set.empty Set.empty

emptyInOut :: BlockInOut a
emptyInOut = BlockInOut Set.empty Set.empty

allocateRegisters :: (Ord a, Register.Register a) => RegAllocContext a -> [IR.Block] -> Set.Set a -> Allocation a
allocateRegisters ctx blocks =
  let
    inOuts = blocksLiveInOut2 blocks
    liveSets' = map (liveSets ctx inOuts) blocks
    interferences' = foldl (\m (b, l) -> Map.union m (interferences ctx b l)) Map.empty (zip blocks liveSets')
  in
    colors interferences'

blocksLiveInOut2 :: (Ord a, Register.Register a) => [IR.Block] -> Map.Map IR.LabelRef (BlockInOut a)
blocksLiveInOut2 = blocksLiveInOut . blocksUseDefs

blocksUseDefs :: (Ord a, Register.Register a) => [IR.Block] -> Map.Map IR.LabelRef (BlockUseDefs a)
blocksUseDefs blocks = Map.fromList $ map (\b -> (IR.blockLabel b, blockUseDefs b)) blocks
  where
    blockUseDefs :: (Ord a0, Register.Register a0) => IR.Block -> BlockUseDefs a0
    blockUseDefs block = snd $ foldl useDef (Set.empty, emptyUseDefs) (reverse $ IR.blockInstructions block)
    useDef :: (Ord a0, Register.Register a0) => (Set.Set (IR.RegEntry a0), BlockUseDefs a0) -> IR.Instruction -> (Set.Set (IR.RegEntry a0), BlockUseDefs a0)
    useDef (live, useDefs) instr =
      let
        defs' = Set.fromList (map IR.Virtual (IRIA.defs instr))
        uses' = Set.fromList (map IR.Virtual (IRIA.uses instr))
        live' = (live Set.\\ defs') `Set.union` uses'
        defs'' = Set.union defs' (defs useDefs)
        successors' = Set.union (Set.fromList (IRIA.successors instr)) (successors useDefs)
        useDefs' = BlockUseDefs live' defs'' successors'
      in
        (live', useDefs')

blocksLiveInOut :: (Ord a, Register.Register a) => Map.Map IR.LabelRef (BlockUseDefs a) -> Map.Map IR.LabelRef (BlockInOut a)
blocksLiveInOut blocks = iterateUntilStable (manyBlocksLiveInOut Map.empty blocks) blocks
  where
    iterateUntilStable :: (Ord a0, Register.Register a0) => Map.Map IR.LabelRef (BlockInOut a0) -> Map.Map IR.LabelRef (BlockUseDefs a0) -> Map.Map IR.LabelRef (BlockInOut a0)
    iterateUntilStable blocks' useDefs =
      let
        blocks'' = manyBlocksLiveInOut blocks' useDefs
        changed = Map.filterWithKey (\k v -> v /= blocks' Map.! k) blocks''
      in
        if Map.null changed then blocks' else iterateUntilStable blocks'' useDefs
    manyBlocksLiveInOut :: (Ord a0, Register.Register a0) => Map.Map IR.LabelRef (BlockInOut a0) -> Map.Map IR.LabelRef (BlockUseDefs a0) -> Map.Map IR.LabelRef (BlockInOut a0)
    manyBlocksLiveInOut init = Map.mapWithKey (\k v -> blockLiveInOut init v)
    blockLiveInOut :: (Ord a0, Register.Register a0) => Map.Map IR.LabelRef (BlockInOut a0) -> BlockUseDefs a0 -> BlockInOut a0
    blockLiveInOut init useDefs =
      let
        liveOut' = Set.unions (Set.map (\s -> liveIn (Map.findWithDefault emptyInOut s init)) (successors useDefs))
        liveIn' = Set.union (Set.difference liveOut' (defs useDefs)) (uses useDefs)
      in
        BlockInOut liveIn' liveOut'

liveSets :: (Ord a, Register.Register a) => RegAllocContext a -> Map.Map IR.LabelRef (BlockInOut a) -> IR.Block -> LiveSets a
liveSets ctx blockInOuts (IR.Block name instructions) = snd $ foldl (liveSet ctx) (liveOut (blockInOuts Map.! name), emptyLiveSets) (reverse instructions)
  where
    liveSet :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> (Set.Set (IR.RegEntry a0), LiveSets a0) -> IR.Instruction -> (Set.Set (IR.RegEntry a0), LiveSets a0)
    liveSet ctx (live, liveSets') instr =
      let
        defs' = Set.fromList (map IR.Virtual (IRIA.defs instr))
        uses' = Set.fromList (map IR.Virtual (IRIA.uses instr))
        live' = (live Set.\\ defs') `Set.union` uses'
        localLive = live' `Set.union` Set.fromList (intLive ctx instr)
        instrs = (live' `Set.union` localLive) : liveInstrs liveSets'
      in
        (live', LiveSets instrs)

interferences :: (Ord a, Register.Register a) => RegAllocContext a -> IR.Block -> LiveSets a -> InterferenceGraph a
interferences ctx block liveSets = foldl (collapseMap ctx) Map.empty (zip (IR.blockInstructions block) (liveInstrs liveSets))
  where
    collapseMap :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> InterferenceGraph a0 -> (IR.Instruction, Set.Set (IR.RegEntry a0)) -> InterferenceGraph a0
    collapseMap ctx map' (instr, live) = insertPhysDefs (insertDefs map' (IRIA.uses instr ++ IRIA.defs instr) live) (intLive ctx instr) live
    insertDefs :: (Ord a0, Register.Register a0) => InterferenceGraph a0 -> [IR.RegName] -> Set.Set (IR.RegEntry a0) -> InterferenceGraph a0
    insertDefs map' uses live = foldl (\m u -> insertDef (live Set.\\ Set.singleton (IR.Virtual u)) m u) map' uses
    insertDef :: (Ord a0, Register.Register a0) => Set.Set (IR.RegEntry a0) -> InterferenceGraph a0 -> IR.RegName -> InterferenceGraph a0
    insertDef live map' use =
      Map.insert use (Set.union live (Map.findWithDefault Set.empty use g)) g
      where g = foldl (\m re -> case re of
              IR.Virtual regName -> Map.insert regName (Set.insert (IR.Virtual use) (Map.findWithDefault Set.empty regName m)) m
              IR.Physical _ -> m) map' live
    insertPhysDefs :: (Ord a0, Register.Register a0) => InterferenceGraph a0 -> [IR.RegEntry a0] -> Set.Set (IR.RegEntry a0) -> InterferenceGraph a0
    insertPhysDefs map' uses live = foldl (\m u -> case u of
        IR.Physical reg -> insertPhysDef (live Set.\\ Set.singleton (IR.Physical reg)) m reg
        _ -> m) map' uses
    insertPhysDef :: (Ord a0, Register.Register a0) => Set.Set (IR.RegEntry a0) -> InterferenceGraph a0 -> a0 -> InterferenceGraph a0
    insertPhysDef live map' use =
      foldl (\m re -> case re of
        IR.Virtual regName -> Map.insert regName (Set.insert (IR.Physical use) (Map.findWithDefault Set.empty regName m)) m
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