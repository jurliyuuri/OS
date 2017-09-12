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
import Data.Bits
import qualified Data.Map as M

type Error = RuntimeError

error' :: String -> VIO a
error' str = do
 (cpu, mem) <- get
 lift . lift . lift . Left . RuntimeError $ str ++ "\nCPU: " ++ show cpu ++ "\nMemory: " ++ show mem


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
  error' $ "f5 register was not preserved after the call. It should be in " ++ show initialF5 ++ " but is actually in " ++ show a

getTat :: VIO (M.Map Word32 (Word32, Instruction))
getTat = tentativeAddressTable <$> ask

getCPU :: VIO CPU
getCPU = fst <$> get

getRegister :: Register -> VIO Word32
getRegister F0 = f0 <$> getCPU
getRegister F1 = f1 <$> getCPU
getRegister F2 = f2 <$> getCPU
getRegister F3 = f3 <$> getCPU
getRegister F5 = f5 <$> getCPU
getRegister XX = xx <$> getCPU

getFlag :: VIO Bool
getFlag = flag <$> getCPU

setFlag :: Bool -> VIO ()
setFlag v = modify $ \(cpu, m) -> (cpu{flag = v},m)

setRegister :: Register -> Word32 -> VIO ()
setRegister F0 v = modify $ \(cpu, m) -> (cpu{f0 = v},m)
setRegister F1 v = modify $ \(cpu, m) -> (cpu{f1 = v},m)
setRegister F2 v = modify $ \(cpu, m) -> (cpu{f2 = v},m)
setRegister F3 v = modify $ \(cpu, m) -> (cpu{f3 = v},m)
setRegister F5 v = modify $ \(cpu, m) -> (cpu{f5 = v},m)
setRegister XX v = modify $ \(cpu, m) -> (cpu{xx = v},m)


executeInstruction :: Instruction -> VIO ()
executeInstruction TERMINATE = error $ "cannot happen"
executeInstruction (Krz r l) = do 
 val1 <- getValueFromR r
 setValueToL l val1 -- cannot use templ here, as templ will *read* from l (and later discards)
executeInstruction (Ata r l) = templ (+) r l
executeInstruction (Nta r l) = templ (-) r l
executeInstruction (Ada r l) = templ (.&.) r l
executeInstruction (Ekc r l) = templ (.|.) r l
executeInstruction (Dal r l) = templ (\x y -> complement $ x `xor` y) r l
executeInstruction (MalKrz r l) = do
 fl <- getFlag
 when fl $ executeInstruction (Krz r l)
executeInstruction (Fi r1 r2 cond) = do
 v1 <- getValueFromR r1
 v2 <- getValueFromR r2
 setFlag $ toFunc cond v1 v2
executeInstruction (Inj r1 l1 l2) = do
 val_r1 <- getValueFromR r1
 val_l1 <- getValueFromR (L l1)
 setValueToL l1 val_r1
 setValueToL l2 val_l1

templ :: (Word32 -> Word32 -> Word32) -> Rvalue -> Lvalue -> VIO ()
templ func r l = do 
 val1 <- getValueFromR r
 val2 <- getValueFromR (L l)
 setValueToL l (func val2 val1)

liftMemOp :: State Memory a -> VIO a
liftMemOp memOp = do
 (cpu, mem) <- get
 let (a, newMem) = memOp `runState` mem
 put $ (cpu, newMem)
 return a

-- data Lvalue = Re Register | RPlusNum Register Word32 | RPlusR Register Register

setValueToL :: Lvalue -> Word32 -> VIO ()
setValueToL (Re reg) dat = setRegister reg dat
setValueToL (RPlusNum register offset) dat = do
 v <- getRegister register
 liftMemOp $ writeM (v + offset) dat
setValueToL (RPlusR r1 r2) dat = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ writeM (v1 + v2) dat

getValueFromR :: Rvalue -> VIO Word32
getValueFromR (Pure word32) = return word32
getValueFromR (Lab label) = do
 lt <- labelTable <$> ask
 case M.lookup label lt of
  Nothing -> error' $ "Undefined label `" ++ label ++ "`"
  Just addr -> return addr
getValueFromR (L (Re register)) = getRegister register
getValueFromR (L (RPlusNum register offset)) = do
 v <- getRegister register
 liftMemOp $ readM (v + offset)
getValueFromR (L (RPlusR r1 r2)) = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ readM (v1 + v2) 

-- nx = xx;
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
    else error' $ "nx has an invalid address " ++ show currentNX
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
 