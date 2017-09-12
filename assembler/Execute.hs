module Execute
(initialCPU
,execute
) where
import Types
import Memory
import TentativeLoad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M

type Error = RuntimeError

data CPU = CPU{ f0 :: Word32, f1 :: Word32, f2 :: Word32, f3 :: Word32, f5 :: Word32, nx :: Word32, xx :: Word32, flag :: Bool, memory :: Memory} deriving (Show, Eq, Ord)

type VIO a = ReaderT TentativeLoad (StateT CPU (Either Error)) a

execute :: TentativeLoad -> Either RuntimeError CPU
execute program = (`execStateT` (initialCPU initialAddress)) $ (`runReaderT` program) $ execute'

execute' :: VIO ()
execute' = do
 instruction <- updateXXAndGetInstruction
 if (instruction == TERMINATE) then finalize else do
  executeInstruction instruction
  updateNX
  execute'

finalize :: VIO ()
finalize = do
 a <- getRegister F5
 if (a /= initialF5) 
  then error $ "f5 register was not preserved after the call. It should be in " ++ show initialF5 ++ " but is actually in " ++ show a
  else return ()

getTat :: VIO (M.Map Word32 (Word32, Instruction))
getTat = tentativeAddressTable <$> ask

getRegister :: Register -> VIO Word32
getRegister F0 = f0 <$> get
getRegister F1 = f1 <$> get
getRegister F2 = f2 <$> get
getRegister F3 = f3 <$> get
getRegister F5 = f5 <$> get
getRegister XX = xx <$> get


executeInstruction :: Instruction -> VIO ()
executeInstruction _ = return () -- undefined

updateNX :: VIO ()
updateNX = do
 currentXX <- getRegister XX
 cpu <- get
 put $ cpu{nx = currentXX}

-- xx = nextAddressOf(nx); return getInstructionFrom(nx);
updateXXAndGetInstruction :: VIO Instruction
updateXXAndGetInstruction = do
 currentNX <- nx <$> get
 tat <- getTat
 case M.lookup currentNX tat of
  Nothing -> 
   if currentNX == outermostRetAddress then return TERMINATE
    else error $ "nx has an invalid address" ++ show currentNX
  Just (newXX, instruction) -> do
   cpu <- get
   put $ cpu{xx = newXX}
   return instruction


initialF5 :: Word32
initialF5 = 0x6d7aa0f8

outermostRetAddress :: Word32
outermostRetAddress = 0xbda574b8

initialMemory :: Memory
initialMemory = writeM initialF5 outermostRetAddress `execState` emptyM

initialCPU :: Word32 -> CPU
initialCPU initNX = CPU{
 f0 = 0x82ebfc85, -- garbage
 f1 = 0xfc73c497, -- garbage
 f2 = 0x9cf84b9d, -- garbage
 f3 = 0x92c073e6, -- garbage
 f5 = initialF5,
 flag = False,
 memory = initialMemory,
 nx = initNX,
 xx = 0xba3decfd -- garbage
 }
 