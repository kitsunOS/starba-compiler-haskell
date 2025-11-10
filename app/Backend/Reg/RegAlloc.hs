module Backend.Reg.RegAlloc where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

import qualified Backend.IR.IR as IR
import qualified Backend.Analysis.IRCfgAnalysis as IRCA
import qualified Backend.Analysis.IRInstrAnalysis as IRIA
import qualified Backend.Reg.Register as Register

newtype LiveSets a = LiveSets {
  liveInstrs :: [Set.Set (IR.RegEntry a)]
} deriving (Show, Eq)

data Allocation a = Allocation {
  allocatedRegisters :: Map.Map IR.RegName a,
  spiltRegisters :: [IR.RegName]
} deriving (Show, Eq)

data RegAllocContext a = RegAllocContext {
  intLive :: IR.Instruction -> [IR.RegEntry a],
  racRegCompat :: IR.RegName -> IR.Instruction -> [a]
}

type InterferenceGraph a = Map.Map IR.RegName (Set.Set (IR.RegEntry a))
type CompatMap a = Map.Map IR.RegName (Set.Set a)

emptyLiveSets :: LiveSets a
emptyLiveSets = LiveSets []

valueToRegName :: IR.Value -> Maybe IR.RegName
valueToRegName (IR.Register regName) = Just regName
valueToRegName _ = Nothing

usesR :: IR.Instruction -> [IR.RegName]
usesR instr = mapMaybe valueToRegName (IRIA.usesV instr)

usesRE :: (Ord a, Register.Register a) => IR.Instruction -> [IR.RegEntry a]
usesRE = map IR.Virtual . usesR

defsR :: IR.Instruction -> [IR.RegName]
defsR instr = mapMaybe valueToRegName (IRIA.defsV instr)

defsRE :: (Ord a, Register.Register a) => IR.Instruction -> [IR.RegEntry a]
defsRE = map IR.Virtual . defsR

cfgaContext :: (Ord a, Register.Register a) => IRCA.CFGAContext (IR.RegEntry a)
cfgaContext = IRCA.CFGAContext {
  IRCA.cfgaUses = usesRE,
  IRCA.cfgaDefs = defsRE
}

allocateRegisters :: (Ord a, Register.Register a, Show a) => RegAllocContext a -> [IR.Block] -> Allocation a
allocateRegisters ctx blocks =
  let
    compatMap' = foldl (\m b -> Map.unionWith Set.union m (compatMap ctx b)) Map.empty blocks
    inOuts = IRCA.cfgLiveInOut cfgaContext (IRCA.cfgBlocks blocks)
    liveSets' = map (liveSets ctx inOuts) blocks
    interferences' = foldl (\m (b, l) -> Map.unionWith Set.union m (interferences ctx b l)) Map.empty (zip blocks liveSets')
  in
    colors interferences' compatMap'

liveSets :: (Ord a, Register.Register a) => RegAllocContext a -> Map.Map IR.LabelRef (IRCA.BlockInOut (IR.RegEntry a)) -> IR.Block -> LiveSets a
liveSets ctx blockInOuts (IR.Block name instructions) = snd $ foldl (liveSet ctx) (IRCA.bioLiveIn (blockInOuts Map.! name), emptyLiveSets) (reverse instructions)
  where
    liveSet :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> (Set.Set (IR.RegEntry a0), LiveSets a0) -> IR.Instruction -> (Set.Set (IR.RegEntry a0), LiveSets a0)
    liveSet ctx (live, liveSets') instr =
      let
        defs' = Set.fromList $ defsRE instr
        uses' = Set.fromList $ usesRE instr
        live' = (live Set.\\ defs') `Set.union` uses'
        localLive = live' `Set.union` Set.fromList (intLive ctx instr)
        instrs = (live' `Set.union` localLive) : liveInstrs liveSets'
      in
        (live', LiveSets instrs)

interferences :: (Ord a, Register.Register a) => RegAllocContext a -> IR.Block -> LiveSets a -> InterferenceGraph a
interferences ctx block liveSets = foldl (collapseMap ctx) Map.empty (zip (IR.blockInstructions block) (liveInstrs liveSets))
  where
    collapseMap :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> InterferenceGraph a0 -> (IR.Instruction, Set.Set (IR.RegEntry a0)) -> InterferenceGraph a0
    collapseMap ctx map' (instr, live) = insertPhysDefs (insertDefs map' (usesR instr ++ defsR instr) live) (intLive ctx instr) live
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

compatMap :: (Ord a, Register.Register a) => RegAllocContext a -> IR.Block -> CompatMap a
compatMap ctx block = foldl (reduceCompat ctx) Map.empty (IR.blockInstructions block)
  where
    reduceCompat :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> CompatMap a0 -> IR.Instruction -> CompatMap a0
    reduceCompat ctx compatMap instr = foldl (\m reg -> reduceReg ctx m reg instr) compatMap (usesR instr ++ defsR instr)
    reduceReg :: (Ord a0, Register.Register a0) => RegAllocContext a0 -> CompatMap a0 -> IR.RegName -> IR.Instruction -> CompatMap a0
    -- If an entry already exists, intersect with racRegCompat, else, set to racRegCompat
    reduceReg ctx compatMap reg instr =
      let compatRegs = Set.fromList (racRegCompat ctx reg instr)
      in case Map.lookup reg compatMap of
        Just avail -> Map.insert reg (avail `Set.intersection` compatRegs) compatMap
        Nothing -> Map.insert reg compatRegs compatMap

colors :: (Ord a, Register.Register a) => InterferenceGraph a -> CompatMap a -> Allocation a
colors graph compatMap = colorWithSpills graph compatMap (Allocation Map.empty [])

colorWithSpills :: (Ord a, Register.Register a) => InterferenceGraph a -> CompatMap a -> Allocation a -> Allocation a
colorWithSpills graph compatMap alloc = case colorNext graph compatMap alloc of
  Left _ -> do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let graph' = graphWithout maxReg graph
    let alloc' = Allocation (allocatedRegisters alloc) (maxReg : spiltRegisters alloc)
    colorWithSpills graph' compatMap alloc'
  Right alloc' -> alloc'

colorNext :: (Ord a, Register.Register a) => InterferenceGraph a -> CompatMap a -> Allocation a -> Either () (Allocation a)
colorNext graph compatMap alloc
  | Map.null graph = Right alloc
  | otherwise = do
    let maxReg = maximumBy (comparing (weightRegName . (graph Map.!))) (Map.keys graph)
    let conflicts = graph Map.! maxReg
    let conflictsPhys = foldl (\s reg -> case reg of
          IR.Virtual regName -> maybe s (`Set.insert` s) (Map.lookup regName (allocatedRegisters alloc))
          IR.Physical reg' -> Set.insert reg' s) Set.empty conflicts
    let avail = compatMap Map.! maxReg Set.\\ conflictsPhys
    if Set.null avail then Left () else do
      let reg32 = head $ Set.toList avail
      let graph' = Map.delete maxReg graph
      let alloc' = Allocation (Map.insert maxReg reg32 (allocatedRegisters alloc)) (spiltRegisters alloc)
      colorNext graph' compatMap alloc'

graphWithout :: (Ord a, Register.Register a) => IR.RegName -> InterferenceGraph a -> InterferenceGraph a
graphWithout reg graph = Map.delete reg $ Map.map (Set.delete (IR.Virtual reg)) graph

-- Lower is better
-- TODO: Maybe account for how many times a regname is used?
weightRegName :: Set.Set (IR.RegEntry a) -> Int
weightRegName = Set.size