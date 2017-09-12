module Execute
(initialHardware
,execute
,CPU()
,Hardware
) where
import Types
import Memory
import TentativeLoad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Map as M

type Error = RuntimeError

data CPU = CPU{ f0 :: Word32, f1 :: Word32, f2 :: Word32, f3 :: Word32, f5 :: Word32, nx :: Word32, xx :: Word32, flag :: Bool} deriving (Show, Eq, Ord)

type Hardware = (CPU, Memory)

type VIO a = WriterT [String] (ReaderT TentativeLoad (StateT Hardware (Either Error))) a

execute :: TentativeLoad -> Either RuntimeError Hardware
execute program = (`execStateT` initialHardware initialAddress) $ (`runReaderT` program) $ runWriterT execute'

execute' :: VIO ()
execute' = do
 instruction <- updateXXAndGetInstruction
 if instruction == TERMINATE then finalize else do
  executeInstruction instruction
  updateNX
  execute'

finalize :: VIO ()
finalize = do
 a <- getRegister F5
 when (a /= initialF5) $
  error $ "f5 register was not preserved after the call. It should be in " ++ show initialF5 ++ " but is actually in " ++ show a

getTat :: VIO (M.Map Word32 (Word32, Instruction))
getTat = tentativeAddressTable <$> ask

getCPU = fst <$> get

getRegister :: Register -> VIO Word32
getRegister F0 = f0 <$> getCPU
getRegister F1 = f1 <$> getCPU
getRegister F2 = f2 <$> getCPU
getRegister F3 = f3 <$> getCPU
getRegister F5 = f5 <$> getCPU
getRegister XX = xx <$> getCPU


executeInstruction :: Instruction -> VIO ()
executeInstruction _ = return () -- undefined

updateNX :: VIO ()
updateNX = do
 currentXX <- getRegister XX
 (cpu, mem) <- get
 put (cpu{nx = currentXX},mem)

-- xx = nextAddressOf(nx); return getInstructionFrom(nx);
updateXXAndGetInstruction :: VIO Instruction
updateXXAndGetInstruction = do
 currentNX <- nx <$> getCPU
 tat <- getTat
 case M.lookup currentNX tat of
  Nothing -> 
   if currentNX == outermostRetAddress then return TERMINATE
    else error $ "nx has an invalid address" ++ show currentNX
  Just (newXX, instruction) -> do
   (cpu, mem) <- get
   put (cpu{xx = newXX}, mem)
   return instruction


initialF5 :: Word32
initialF5 = 0x6d7aa0f8

outermostRetAddress :: Word32
outermostRetAddress = 0xbda574b8

initialMemory :: Memory
initialMemory = writeM initialF5 outermostRetAddress `execState` emptyM

initialHardware :: Word32 -> Hardware
initialHardware initNX = (CPU{
 f0 = 0x82ebfc85, -- garbage
 f1 = 0xfc73c497, -- garbage
 f2 = 0x9cf84b9d, -- garbage
 f3 = 0x92c073e6, -- garbage
 f5 = initialF5,
 flag = False,
 nx = initNX,
 xx = 0xba3decfd -- garbage
 }, initialMemory)
 