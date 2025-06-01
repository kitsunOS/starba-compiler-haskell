module IR.IRPhiGen (phiGen) where

import IR.IR (Block (blockInstructions, blockLabel, Block), LabelRef (LabelRef), Instruction (Jmp, JmpIf, Phi), RegName (RegName))
import IR.IRInstrAnalysis (usesV, defsV, successors)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (Except, runExcept, MonadError (throwError))
import Control.Monad.State (StateT (runStateT), gets, modify)
import qualified IR.IR as IR

data OrderedBlock = OrderedBlock {
  obBlock :: Block,
  obSuccessors :: Set.Set LabelRef,
  obRequestedVars :: Set.Set IR.Value,
  obDefinedVars :: Set.Set IR.Value
} deriving (Show, Eq)

type RegSources = Map.Map LabelRef (Map.Map IR.Value (Set.Set LabelRef))

data PhiGenD = PhiGenD {
  pgNextRegister :: RegName,
  pgActiveRemaps :: Map.Map IR.Value (Map.Map LabelRef RegName),
  pgRegSources :: RegSources,
  pgSourceBlocks :: Map.Map LabelRef OrderedBlock
}

type PhiGenM = StateT PhiGenD (Except String)

type PhiOuts = Map.Map IR.Value IR.RegName

blankOrderedBlock :: Block -> OrderedBlock
blankOrderedBlock block = OrderedBlock {
  obBlock = block,
  obSuccessors = Set.empty,
  obRequestedVars = Set.empty,
  obDefinedVars = Set.empty
}

orderBlocks :: [Block] -> [OrderedBlock]
orderBlocks = map (\b -> (blankOrderedBlock b) { obSuccessors = Set.fromList (allInstructionSuccessors b) })
  where
    allInstructionSuccessors :: Block -> [LabelRef]
    allInstructionSuccessors block = concatMap successors (blockInstructions block)

usedRegs :: Block -> (Set.Set IR.Value, Set.Set IR.Value)
usedRegs block = foldl usedRegs' (Set.empty, Set.empty) (reverse (blockInstructions block))
  where
    usedRegs' :: (Set.Set IR.Value, Set.Set IR.Value) -> Instruction -> (Set.Set IR.Value, Set.Set IR.Value)
    usedRegs' (usedAcc, defAcc) inst =
      let used = Set.fromList (usesV inst)
          defs = Set.fromList (defsV inst)
      in (Set.difference (Set.union usedAcc used) defs, Set.union defAcc defs)

determineRegSources :: [OrderedBlock] -> RegSources
determineRegSources blocks =
  let blocks' = map blockReqDef blocks
  in mergeRegSources $ map (determineRegSources' $ blocksMap blocks') blocks'
  where
    determineRegSources' :: Map.Map LabelRef OrderedBlock -> OrderedBlock -> RegSources
    determineRegSources' blocks block = foldl
      (\acc regName -> propogateRegSource blocks block regName (blockLabel $ obBlock block) Set.empty acc)
      Map.empty (Set.toList (obDefinedVars block))

    blockReqDef :: OrderedBlock -> OrderedBlock
    blockReqDef block =
      let (used, defs) = usedRegs (obBlock block)
      in block { obRequestedVars = used, obDefinedVars = defs }

    blocksMap :: [OrderedBlock] -> Map.Map LabelRef OrderedBlock
    blocksMap blocks = Map.fromList [(blockLabel (obBlock block), block) | block <- blocks]

    propogateRegSource :: Map.Map LabelRef OrderedBlock -> OrderedBlock -> IR.Value -> LabelRef -> Set.Set LabelRef -> RegSources -> RegSources
    propogateRegSource blocks block regName source visited sources =
      let blockLabel' = blockLabel (obBlock block)
          visited' = Set.insert blockLabel' visited
          hasReg = Set.member regName (obRequestedVars block)
          sources' = if hasReg
            then Map.insertWith (Map.unionWith Set.union) blockLabel' (Map.singleton regName (Set.singleton source)) sources
            else sources
          hasDef = Set.member regName (obDefinedVars block)
      in if hasDef && blockLabel' /= source
        then sources'
        else foldl (\acc nextBlockLabel ->
          if Set.member blockLabel' visited
          then acc
          else propogateRegSource blocks (blocks Map.! nextBlockLabel) regName source visited' acc
        ) sources' (Set.toList (obSuccessors block))

    mergeRegSources :: [RegSources] -> RegSources
    mergeRegSources = foldl (Map.unionWith (Map.unionWith Set.union)) Map.empty

getNextRegister :: PhiGenM RegName
getNextRegister = do
  regName <- gets pgNextRegister
  modify $ \s -> s { pgNextRegister = nextRegName regName }
  return regName

-- TODO: Deduplicate logic
nextRegName :: RegName -> RegName
nextRegName (RegName prefix num) =
  RegName (incrementName prefix) 0

incrementName :: String -> String
incrementName = reverse . increment . reverse
  where
    increment [] = ['a']
    increment ('z':xs) = 'a' : increment xs
    increment (x:xs) = succ x : xs

remapRegisterName :: IR.Value -> OrderedBlock -> PhiGenM RegName
remapRegisterName regName block = do
  regSources <- gets pgRegSources
  sourceBlocks <- gets pgSourceBlocks
  let blockLabel' = blockLabel (obBlock block)
  case Map.lookup blockLabel' regSources of
    Just bRegSources -> case Map.lookup regName bRegSources of
      Just sourceLabels -> do
        case Set.size sourceLabels of
          0 -> getNextRegister -- No register to use
          1 -> mappedRegister regName (sourceBlocks Map.! Set.findMin sourceLabels) -- Piggyback on existing source
          _ -> getNextRegister -- Phi into a new register
      Nothing -> getNextRegister
    Nothing -> getNextRegister

remapRegister :: IR.Value -> OrderedBlock -> PhiGenM RegName
remapRegister regName block = do
  activeRemaps <- gets pgActiveRemaps
  let blockLabel' = blockLabel (obBlock block)
  newName <- remapRegisterName regName block
  let newRemap = Map.singleton blockLabel' newName
  let newRemaps = Map.insertWith Map.union regName newRemap activeRemaps
  modify $ \s -> s { pgActiveRemaps = newRemaps }
  return newName

mappedRegister :: IR.Value -> OrderedBlock -> PhiGenM RegName
mappedRegister regName block = do
  activeRemaps <- gets pgActiveRemaps
  let blockLabel' = blockLabel (obBlock block)
  (activeRemaps', mapping) <- case Map.lookup regName activeRemaps of
    Just remaps -> case Map.lookup blockLabel' remaps of
      Just mappedReg -> return (activeRemaps, mappedReg)
      Nothing -> do
        newReg <- remapRegisterName regName block
        return (Map.insertWith Map.union regName (Map.singleton blockLabel' newReg) activeRemaps, newReg)
    Nothing -> do
      newReg <- getNextRegister
      let newLabelRemaps = Map.singleton blockLabel' newReg
      return (Map.insert regName newLabelRemaps activeRemaps, newReg)
  modify $ \s -> s { pgActiveRemaps = activeRemaps' }
  return mapping

rewriteBlock :: OrderedBlock -> PhiGenM (Block, PhiOuts)
rewriteBlock block = do
  let blockLabel' = blockLabel (obBlock block)
  phiOuts <- generatePhiOuts block
  newInstructions <- mapM (rewriteInstruction block) (blockInstructions (obBlock block))
  return (Block { blockLabel = blockLabel', blockInstructions = newInstructions }, phiOuts)

  where
    generatePhiOuts :: OrderedBlock -> PhiGenM PhiOuts
    generatePhiOuts block = do
      regSources <- gets pgRegSources
      let blockLabel' = blockLabel (obBlock block)
      case Map.lookup blockLabel' regSources of
        Just bRegSources -> do
          allMainVars <- mapM (\(k, v) -> mappedRegister k block >>= \mainVar ->
            return (k, mainVar)) (Map.toList bRegSources)
          return $ Map.fromList allMainVars
        Nothing -> return Map.empty

    rewriteInstruction :: OrderedBlock -> Instruction -> PhiGenM Instruction
    rewriteInstruction block (IR.Ret Nothing) = return $ IR.Ret Nothing
    rewriteInstruction block (IR.Ret (Just value)) = do
      remappedValue <- mappedValue value block
      return $ IR.Ret (Just remappedValue)
    rewriteInstruction block (IR.Set dest src) = do
      remappedDest <- remapValue dest block
      remappedSrc <- mappedValue src block
      return $ IR.Set remappedDest remappedSrc
    rewriteInstruction block (IR.BinOp op dest src1 src2) = do
      remappedDest <- remapValue dest block
      remappedSrc1 <- mappedValue src1 block
      remappedSrc2 <- mappedValue src2 block
      return $ IR.BinOp op remappedDest remappedSrc1 remappedSrc2
    rewriteInstruction block (IR.Jmp label) = return $ IR.Jmp label
    rewriteInstruction block (IR.JmpIf cond label1 label2) = do
      remappedCond <- mappedValue cond block
      return $ IR.JmpIf remappedCond label1 label2
    rewriteInstruction block (Phi regName operands) = do
      remappedOperands <- mapM (\(label, val) -> do
        remappedReg <- mappedValue val block
        return (label, remappedReg)) operands
      return $ Phi regName remappedOperands

prependPhiOuts :: Block -> PhiOuts -> PhiGenM Block
prependPhiOuts block phiOuts = do
  newHeader <- generatePhis block phiOuts
  let newInstructions = blockInstructions block
  return $ block { blockInstructions = newHeader ++ newInstructions }

  where
    generatePhis :: Block -> PhiOuts -> PhiGenM [Instruction]
    generatePhis block phiOuts = do
      regSources <- gets pgRegSources
      let blockLabel' = blockLabel block
      case Map.lookup blockLabel' regSources of
        Just bRegSources -> do
          mapM (\(k, v) -> generatePhiOperands k v >>= \ops ->
            case Map.lookup k phiOuts of
              Just mainVar -> return $ Phi mainVar ops
              Nothing -> throwError $ "No main variable found for " ++ show k) (Map.toList bRegSources)
        Nothing -> return []

    generatePhiOperands :: IR.Value -> Set.Set LabelRef -> PhiGenM [(LabelRef, IR.Value)]
    generatePhiOperands regName sourceLabels = do
      sourceBlocks <- gets pgSourceBlocks
      mapM (\label -> do
        let sourceBlock = sourceBlocks Map.! label
        remappedVal <- mappedValue regName sourceBlock
        return (label, remappedVal)) (Set.toList sourceLabels)

remapValue :: IR.Value -> OrderedBlock -> PhiGenM IR.Value
remapValue (IR.Register regName) block = do
  remappedReg <- remapRegister (IR.Register regName) block
  return $ IR.Register remappedReg
remapValue (IR.VariableReference varRef) block = do
  remappedVar <- remapRegister (IR.VariableReference varRef) block
  return $ IR.Register remappedVar
remapValue a _ = return a

mappedValue :: IR.Value -> OrderedBlock -> PhiGenM IR.Value
mappedValue (IR.Register regName) block = do
  remappedReg <- mappedRegister (IR.Register regName) block
  return $ IR.Register remappedReg
mappedValue (IR.VariableReference varRef) block = do
  remappedVar <- mappedRegister (IR.VariableReference varRef) block
  return $ IR.Register remappedVar
mappedValue a _ = return a

rewriteBlocks :: [OrderedBlock] -> PhiGenM [Block]
rewriteBlocks blocks = do
  regSources <- gets pgRegSources
  sourceBlocks <- gets pgSourceBlocks
  let blocksMap = Map.fromList [(blockLabel (obBlock b), b) | b <- blocks]
  modify $ \s -> s { pgRegSources = regSources, pgSourceBlocks = sourceBlocks }
  blocksAndPhiOuts <- mapM rewriteBlock blocks
  mapM (uncurry prependPhiOuts) blocksAndPhiOuts

phiGenBlocks :: [Block] -> Except String [Block]
phiGenBlocks blocks = do
  let orderedBlocks = orderBlocks blocks
      regSources = determineRegSources orderedBlocks
      initialState = PhiGenD {
        pgNextRegister = RegName "a" 0,
        pgActiveRemaps = Map.empty,
        pgRegSources = regSources,
        pgSourceBlocks = Map.fromList [(blockLabel (obBlock b), b) | b <- orderedBlocks]
      }
  evalStateT (rewriteBlocks orderedBlocks) initialState
  where
    evalStateT :: PhiGenM a -> PhiGenD -> Except String a
    evalStateT action initialState =
      case runExcept (runStateT action initialState) of
        Left err -> throwError err
        Right (value, _) -> return value

phiGenProcedure :: IR.Procedure -> Except String IR.Procedure
phiGenProcedure (IR.Procedure blocks) = do
  newBlocks <- phiGenBlocks blocks
  return $ IR.Procedure newBlocks

phiGen :: IR.Module -> Either String IR.Module
phiGen (IR.Module procedures fieldTable symbolTable) = do
  newProcedures <- mapM (runExcept . phiGenProcedure) procedures
  return $ IR.Module newProcedures fieldTable symbolTable