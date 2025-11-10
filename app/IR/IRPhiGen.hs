module IR.IRPhiGen (phiGen) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except (Except, runExcept, MonadError (throwError))
import Control.Monad.State (StateT (runStateT), gets, modify, mapStateT, evalStateT)
import qualified IR.IR as IR
import qualified IR.IRCfgAnalysis as IRCA
import qualified IR.IRInstrAnalysis as IRIA
-- TODO: Break dependency on AST
import qualified AST.AST as AST

type ValueRemaps = Map.Map IR.Value IR.Value;

-- TODO: Optimize when phis are generated
-- For right now, rely on IRValueProp to do that...

data PGBlock = PGBlock {
  pgbCFGBlock :: IRCA.CFGBlock,
  pgbInOut :: IRCA.BlockInOut IR.Value,
  pgbValueRemaps :: ValueRemaps,
  pgbPhiSourceNames :: Map.Map IR.RegName IR.VarRef
} deriving (Show, Eq)

data PhiGenD = PhiGenD {
  pgLastRegisterVer :: Map.Map IR.Value Int,
  pgSourceBlocks :: Map.Map IR.LabelRef PGBlock,
  pgActiveBlock :: PGBlock
}

type PhiGenM = StateT PhiGenD (Except String)

activeBlockLabel :: PhiGenM IR.LabelRef
activeBlockLabel = gets $ IR.blockLabel . IRCA.obBlock . pgbCFGBlock . pgActiveBlock

-- TODO: Ensure it does not conflict with existing registers
newMapping :: IR.Value -> PhiGenM IR.Value
newMapping (IR.VariableReference (AST.Symbol name version)) = do
  let oldValue = IR.VariableReference $ AST.Symbol name version
  newRegisterVer <- gets (\s -> case Map.lookup oldValue (pgLastRegisterVer s) of
    Just v -> v + 1
    Nothing -> 0)
  let newValue = IR.Register $ IR.RegName ("_"++ name ++ "_"  ++ show version) newRegisterVer
  modify (\s -> s {
    pgLastRegisterVer = Map.insert oldValue newRegisterVer $ pgLastRegisterVer s,
    pgActiveBlock = (pgActiveBlock s) {
      pgbValueRemaps = Map.insert oldValue newValue ((pgbValueRemaps . pgActiveBlock) s)
    }
  })
  return newValue

newMapping v = pure v

activeMapping :: IR.Value -> PhiGenM IR.Value
activeMapping (IR.VariableReference symbol) = do
  let value = IR.VariableReference symbol
  activeBlock <- gets pgActiveBlock
  sourceBlocks <- gets pgSourceBlocks
  return $ pgbValueRemaps activeBlock Map.! value

activeMapping value = pure value

rewriteInstruction :: IR.Instruction -> PhiGenM IR.Instruction
rewriteInstruction (IR.Ret Nothing) = return $ IR.Ret Nothing
rewriteInstruction (IR.Ret (Just value)) = do
  remappedRet <- activeMapping value
  return $ IR.Ret (Just remappedRet)
rewriteInstruction (IR.Set dest src) = do
  remappedDest <- newMapping dest
  remappedSrc <- activeMapping src
  return $ IR.Set remappedDest remappedSrc
rewriteInstruction (IR.BinOp op dest src1 src2) = do
  remappedDest <- newMapping dest
  remappedSrc1 <- activeMapping src1
  remappedSrc2 <- activeMapping src2
  return $ IR.BinOp op remappedDest remappedSrc1 remappedSrc2
rewriteInstruction (IR.Jmp label) = return $ IR.Jmp label
rewriteInstruction (IR.JmpIf cond label1 label2) = do
  remappedCond <- activeMapping cond
  return $ IR.JmpIf remappedCond label1 label2
rewriteInstruction (IR.Phi regName operands) = do
  remappedOperands <- mapM (\(label, val) -> do
    remappedReg <- activeMapping val
    return (label, remappedReg)) operands
  return $ IR.Phi regName remappedOperands

captureInitialActiveValueBlock :: IR.Value -> PhiGenM ()
captureInitialActiveValueBlock (IR.VariableReference (AST.Symbol name version)) = do
  let varRef = AST.Symbol name version
  let oldValue = IR.VariableReference varRef
  predecessors <- gets $ IRCA.obPredecessors . pgbCFGBlock . pgActiveBlock
  if Set.size predecessors > 1
    then do
      nextMapping <- newMapping oldValue
      case nextMapping of
        IR.Register regName -> modify (\s -> s {
          pgActiveBlock = (pgActiveBlock s) {
            pgbPhiSourceNames = Map.insert regName varRef ((pgbPhiSourceNames . pgActiveBlock) s)
          }
        })
        _ -> pure ()
    else do
      modify (\s ->
        let
          sourceLabel = head $ Set.toList predecessors
          currentValue = pgbValueRemaps (pgSourceBlocks s Map.! sourceLabel) Map.! oldValue
        in s {
          pgActiveBlock = (pgActiveBlock s) {
            pgbValueRemaps = Map.insert oldValue currentValue ((pgbValueRemaps . pgActiveBlock) s)
          }
        })
  return ()
captureInitialActiveValueBlock v = pure ()

captureInitialActiveValuesBlock :: PhiGenM ()
captureInitialActiveValuesBlock = do
  inValues <- gets $ IRCA.bioLiveIn . pgbInOut . pgActiveBlock
  mapM_ captureInitialActiveValueBlock $ Set.toList inValues

modifyBlockInstructions :: ([IR.Instruction] -> PhiGenM [IR.Instruction]) -> IR.LabelRef -> PhiGenM IR.Block
modifyBlockInstructions instructionsGen labelRef = do
  modify (\s -> s { pgActiveBlock = pgSourceBlocks s Map.! labelRef })
  activeBlockInstrs <- gets $ IR.blockInstructions . IRCA.obBlock . pgbCFGBlock . pgActiveBlock
  newInstructions <- instructionsGen activeBlockInstrs
  modify (\s -> s { pgActiveBlock = (pgActiveBlock s) { -- This is pain...
    pgbCFGBlock = (pgbCFGBlock . pgActiveBlock $ s) {
      IRCA.obBlock = (IRCA.obBlock . pgbCFGBlock . pgActiveBlock $ s) {
        IR.blockInstructions = newInstructions
      }
    }
  } })
  modify (\s -> s { pgSourceBlocks = Map.insert labelRef (pgActiveBlock s) (pgSourceBlocks s) })
  gets $ IRCA.obBlock . pgbCFGBlock . pgActiveBlock

prePhiBlockGen :: IR.LabelRef -> PhiGenM IR.Block
prePhiBlockGen = modifyBlockInstructions (\activeBlockInstrs -> do
  captureInitialActiveValuesBlock
  mapM rewriteInstruction activeBlockInstrs)

phiValueGen :: IR.RegName -> IR.VarRef -> PhiGenM IR.Instruction
phiValueGen regName varRef = do
  sourceBlocks <- gets pgSourceBlocks
  predecessors <- gets $ IRCA.obPredecessors . pgbCFGBlock . pgActiveBlock
  let sourceValues = map (\pred -> (pred, pgbValueRemaps (sourceBlocks Map.! pred) Map.! IR.VariableReference varRef)) $ Set.toList predecessors
  return $ IR.Phi regName sourceValues

phiBlockGen :: IR.LabelRef -> PhiGenM IR.Block
phiBlockGen = modifyBlockInstructions (\activeBlockInstrs -> do
  sourceNames <- gets $ pgbPhiSourceNames . pgActiveBlock
  phiInstructions <- mapM (uncurry phiValueGen) $ Map.toList sourceNames
  return $ phiInstructions ++ activeBlockInstrs)

phiGen :: IR.Procedure -> Either String IR.Procedure
phiGen procedure = do
  let entryLabel = IR.LabelRef "entry" -- TOD: Proper entry label
  -- TODO: Re-use a pre-computed CFG
  let cfg = IRCA.generateCfg procedure
  let blockOrder = map (IR.blockLabel . IRCA.obBlock) $ IRCA.cfgReversePostOrder cfg entryLabel

  let ctx = IRCA.CFGAContext {
    IRCA.cfgaUses = IRIA.usesV,
    IRCA.cfgaDefs = IRIA.defsV
  }
  let inOuts = IRCA.cfgLiveInOut ctx cfg
  let pgSourceBlocks = Map.mapWithKey (\labelRef cfgBlock -> PGBlock {
    pgbCFGBlock = cfgBlock,
    pgbInOut = inOuts Map.! labelRef,
    pgbValueRemaps = Map.empty,
    pgbPhiSourceNames = Map.empty
  }) cfg
  let phiGenState = PhiGenD {
    pgLastRegisterVer = Map.empty,
    pgSourceBlocks = pgSourceBlocks,
    pgActiveBlock = pgSourceBlocks Map.! entryLabel
  }
  let genBlocks = mapM_ prePhiBlockGen blockOrder >> mapM phiBlockGen blockOrder

  case runExcept (evalStateT genBlocks phiGenState) of
    Left err -> Left err
    Right res -> Right $ IR.Procedure res