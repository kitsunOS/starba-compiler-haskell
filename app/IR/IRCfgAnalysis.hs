module IR.IRCfgAnalysis (
  ControlFlowGraph,
  CFGBlock (..),
  BlockInOut (..),
  CFGAContext (..),
  generateCfg, cfgReversePostOrder, cfgLiveInOuts
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified IR.IR as IR
import qualified IR.IRInstrAnalysis as IRIA
import Data.Bifunctor (Bifunctor(first))

data CFGBlock = CFGBlock {
  -- TODO: Better to store an IR.LabelRef?
  obBlock :: IR.Block,
  obSuccessors :: Set.Set IR.LabelRef,
  obPredecessors :: Set.Set IR.LabelRef
} deriving (Show, Eq)

type ControlFlowGraph = Map.Map IR.LabelRef CFGBlock

cfgSuccessors :: IR.Block -> Set.Set IR.LabelRef
cfgSuccessors block = foldl (\acc instr -> acc `Set.union` Set.fromList (IRIA.successors instr)) Set.empty (IR.blockInstructions block)

cfgBlocks :: [IR.Block] -> ControlFlowGraph
cfgBlocks = cfgWithPreds . cfgWithSucs
  where
    cfgWithPreds :: ControlFlowGraph -> ControlFlowGraph
    cfgWithPreds cfg = foldl (\acc mapK ->
      foldl (flip (Map.update (\cfgBlock -> Just $ cfgBlock { obPredecessors = obPredecessors cfgBlock `Set.union` Set.singleton mapK}))) acc (obSuccessors $ cfg Map.! mapK)
      ) cfg (Map.keys cfg)

    cfgWithSucs :: [IR.Block] -> ControlFlowGraph
    cfgWithSucs = foldl (\acc block ->
      let newBlock = CFGBlock { obBlock = block, obSuccessors = cfgSuccessors block, obPredecessors = Set.empty }
      in Map.insert (IR.blockLabel block) newBlock acc
      ) Map.empty

cfgReversePostOrder :: ControlFlowGraph -> IR.LabelRef -> [CFGBlock]
cfgReversePostOrder cfg entryLabel = fst $ cfgPostOrder cfg ([], []) (cfg Map.! entryLabel)
  where
    -- Sadly can't use a set because no Ord. Hopefully not too slow :/
    cfgPostOrder :: ControlFlowGraph -> ([CFGBlock], [CFGBlock]) -> CFGBlock -> ([CFGBlock], [CFGBlock])
    cfgPostOrder cfg (postOrder, visited) current =
      if current `elem` visited
        then (postOrder, visited)
        else let rst1 = foldl (cfgPostOrder cfg) (postOrder, current:visited) (map (cfg Map.!) (Set.toList $ obSuccessors current))
          in first (current :) rst1

generateCfg :: IR.Procedure -> ControlFlowGraph
generateCfg (IR.Procedure blocks) = cfgBlocks blocks

---

-- TODO: Change RegAlloc to use this
data BlockUseDefs a = BlockUseDefs {
  budBlock :: CFGBlock,
  budUses :: Set.Set a,
  budDefs :: Set.Set a
} deriving (Show, Eq)

data BlockInOut a = BlockInOut {
  bioLiveIn :: Set.Set a,
  bioLiveOut :: Set.Set a
} deriving (Show, Eq)

data CFGAContext a = CFGAContext {
  cfgaUses :: IR.Instruction -> [a],
  cfgaDefs :: IR.Instruction -> [a]
}

emptyUseDefs :: CFGBlock -> BlockUseDefs a
emptyUseDefs cfgBlock = BlockUseDefs cfgBlock Set.empty Set.empty

emptyInOut :: BlockInOut a
emptyInOut = BlockInOut Set.empty Set.empty

blocksUseDefs :: Ord a => CFGAContext a -> [CFGBlock] -> Map.Map IR.LabelRef (BlockUseDefs a)
blocksUseDefs ctx blocks = Map.fromList $ map (\b -> (IR.blockLabel (obBlock b), blockUseDefs ctx b)) blocks
  where
    blockUseDefs :: Ord a0 => CFGAContext a0 -> CFGBlock -> BlockUseDefs a0
    blockUseDefs ctx block = snd $ foldl (useDef ctx) (Set.empty, emptyUseDefs block) (reverse $ IR.blockInstructions (obBlock block))

    useDef :: Ord a0 => CFGAContext a0 -> (Set.Set a0, BlockUseDefs a0) -> IR.Instruction -> (Set.Set a0, BlockUseDefs a0)
    useDef ctx (live, useDefs) instr =
      let
        defs' = Set.fromList $ cfgaDefs ctx instr
        uses' = Set.fromList $ cfgaUses ctx instr
        live' = (live Set.\\ defs') `Set.union` uses'
        defs'' = Set.union defs' (budDefs useDefs)
        successors' = Set.union (Set.fromList (IRIA.successors instr)) (obSuccessors (budBlock useDefs))
        useDefs' = useDefs { budUses = live', budDefs = defs'' }
      in
        (live', useDefs')


blocksLiveInOut :: (Ord a, Eq a) => CFGAContext a -> [CFGBlock] -> Map.Map IR.LabelRef (BlockInOut a)
blocksLiveInOut ctx = blocksLiveInOut' . blocksUseDefs ctx

blocksLiveInOut' :: (Ord a, Eq a) => Map.Map IR.LabelRef (BlockUseDefs a) -> Map.Map IR.LabelRef (BlockInOut a)
blocksLiveInOut' blocks = iterateUntilStable (manyBlocksLiveInOut Map.empty blocks) blocks
  where
    iterateUntilStable :: (Ord a0, Eq a0) => Map.Map IR.LabelRef (BlockInOut a0) -> Map.Map IR.LabelRef (BlockUseDefs a0) -> Map.Map IR.LabelRef (BlockInOut a0)
    iterateUntilStable blocks' useDefs =
      let
        blocks'' = manyBlocksLiveInOut blocks' useDefs
        changed = Map.filterWithKey (\k v -> v /= blocks' Map.! k) blocks''
      in
        if Map.null changed then blocks' else iterateUntilStable blocks'' useDefs

    manyBlocksLiveInOut :: (Ord a0, Eq a0) => Map.Map IR.LabelRef (BlockInOut a0) -> Map.Map IR.LabelRef (BlockUseDefs a0) -> Map.Map IR.LabelRef (BlockInOut a0)
    manyBlocksLiveInOut init = Map.mapWithKey (\k v -> blockLiveInOut init v)

    blockLiveInOut :: (Ord a0, Eq a0) => Map.Map IR.LabelRef (BlockInOut a0) -> BlockUseDefs a0 -> BlockInOut a0
    blockLiveInOut init useDefs =
      let
        liveOut' = Set.unions (Set.map (\s -> bioLiveIn (Map.findWithDefault emptyInOut s init)) (obSuccessors $ budBlock useDefs))
        liveIn' = Set.union (Set.difference liveOut' (budDefs useDefs)) (budUses useDefs)
      in
        BlockInOut liveIn' liveOut'


cfgLiveInOuts :: Ord a => CFGAContext a -> ControlFlowGraph -> Map.Map IR.LabelRef (BlockInOut a)
cfgLiveInOuts ctx cfg = blocksLiveInOut ctx (Map.elems cfg)