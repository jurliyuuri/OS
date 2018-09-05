{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}
module Execute
(initialHardware
,execute
,unwrapWith
,CPU()
,Hardware
,execOne
) where
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Bits
import Data.Word
import Data.Int

import Types
import Memory
import Linker


type Error = RuntimeError

runtimeError :: String -> VIO a
runtimeError str = do
 (cpu, mem) <- get
 lift . lift . throwError . RuntimeError str $ "CPU: " ++ show cpu ++ "\nMemory: " ++ show mem



data CPU = CPU{ f0 :: Word32, f1 :: Word32, f2 :: Word32, f3 :: Word32, f5 :: Word32, nx :: Word32, xx :: Word32, flag :: Bool} deriving (Show, Eq, Ord)

type Hardware = (CPU, Memory)
type Logs = [String]

type VIO a = ReaderT Program (StateT Hardware (ExceptT Error (Writer Logs))) a

execute :: Program -> (Either RuntimeError (Bool, Hardware), Logs)
execute program = unwrapWith (initialHardware initialAddress, program) execute'

unwrapWith :: (Hardware, Program) -> VIO Bool -> (Either Error (Bool, Hardware), Logs)
unwrapWith (initHW, program) = runWriter 
 . runExceptT
 . (`runStateT` initHW) 
 . (`runReaderT` program) 


execute' :: VIO Bool
execute' = fix execOne

execOne :: VIO Bool -> VIO Bool
execOne f = do
 instruction <- updateXXAndGetInstruction
 if instruction == TERMINATE then finalize else do
  executeInstruction instruction
  updateNX
  f

finalize :: VIO Bool
finalize = do
 a <- getRegister F5
 if a /= initialF5
  then runtimeError $ "f5 register was not preserved after the call. It should be in " ++ show initialF5 ++ " but is actually in " ++ show a
  else return False


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

setHigh8bitOfRegister :: Register -> Word8 -> VIO ()
setHigh8bitOfRegister F0 v = modify $ \(cpu, m) -> (cpu{f0 = modifyHigh8bit v (f0 cpu)},m)
setHigh8bitOfRegister F1 v = modify $ \(cpu, m) -> (cpu{f1 = modifyHigh8bit v (f1 cpu)},m)
setHigh8bitOfRegister F2 v = modify $ \(cpu, m) -> (cpu{f2 = modifyHigh8bit v (f2 cpu)},m)
setHigh8bitOfRegister F3 v = modify $ \(cpu, m) -> (cpu{f3 = modifyHigh8bit v (f3 cpu)},m)
setHigh8bitOfRegister F5 v = modify $ \(cpu, m) -> (cpu{f5 = modifyHigh8bit v (f5 cpu)},m)
setHigh8bitOfRegister XX v = modify $ \(cpu, m) -> (cpu{xx = modifyHigh8bit v (xx cpu)},m)

modifyHigh8bit :: Word8 -> Word32 -> Word32
modifyHigh8bit a b = shiftL (fromIntegral a) (32 - 8) .|. (b .&. 0x00ffffff) 


setHigh16bitOfRegister :: Register -> Word16 -> VIO ()
setHigh16bitOfRegister F0 v = modify $ \(cpu, m) -> (cpu{f0 = modifyHigh16bit v (f0 cpu)},m)
setHigh16bitOfRegister F1 v = modify $ \(cpu, m) -> (cpu{f1 = modifyHigh16bit v (f1 cpu)},m)
setHigh16bitOfRegister F2 v = modify $ \(cpu, m) -> (cpu{f2 = modifyHigh16bit v (f2 cpu)},m)
setHigh16bitOfRegister F3 v = modify $ \(cpu, m) -> (cpu{f3 = modifyHigh16bit v (f3 cpu)},m)
setHigh16bitOfRegister F5 v = modify $ \(cpu, m) -> (cpu{f5 = modifyHigh16bit v (f5 cpu)},m)
setHigh16bitOfRegister XX v = modify $ \(cpu, m) -> (cpu{xx = modifyHigh16bit v (xx cpu)},m)

modifyHigh16bit :: Word16 -> Word32 -> Word32
modifyHigh16bit a b = shiftL (fromIntegral a) (32 - 16) .|. (b .&. 0x0000ffff) 

dtosna :: Word32 -> Word32 -> Word32
dtosna x y = fromIntegral $ x' `shift` negate (fromIntegral y)
 where x' = fromIntegral x :: Int32 

set64bitProd :: (Lvalue, Lvalue) -> Word64 ->  VIO ()
set64bitProd (lh, ll) prod = do
 let higher = fromIntegral $ prod `shift` (-32) :: Word32
 let lower = fromIntegral prod :: Word32
 setValueToL lh higher
 setValueToL ll lower 

executeInstruction :: Instruction -> VIO ()
executeInstruction TERMINATE = error "cannot happen"
executeInstruction (Krz r l) = do 
 val1 <- getValueFromR r
 setValueToL l val1 -- cannot use templ here, as templ will *read* from l (and later discards)
executeInstruction (Ata r l) = templ (+) r l
executeInstruction (Nta r l) = templ (-) r l
executeInstruction (Ada r l) = templ (.&.) r l
executeInstruction (Ekc r l) = templ (.|.) r l
executeInstruction (Dal r l) = templ (\x y -> complement $ x `xor` y) r l
executeInstruction (Dto r l) = templ (\x y -> x `shift` negate (fromIntegral y)) r l
executeInstruction (Dtosna r l) = templ dtosna r l
executeInstruction (Dro r l) = templ (\x y -> x `shift` fromIntegral y) r l
executeInstruction (Lat r ll lh) = do
 v1 <- getValueFromR r
 v2 <- getValueFromR (L ll)
 let prod = (fromIntegral v1::Word64) * (fromIntegral v2::Word64)
 set64bitProd (lh, ll) prod
executeInstruction (Latsna r ll lh) = do
 v1 <- getValueFromR r
 v2 <- getValueFromR (L ll)
 let prod = (fromIntegral (fromIntegral v1::Int32)::Word64) * (fromIntegral (fromIntegral v2::Int32)::Word64)
 set64bitProd (lh, ll) prod 
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
executeInstruction (Krz8i r l) = do {- load highest 8bit from r; sign-extend; write to l -}
 val1 <- signExtendFrom8 <$> getHighest8bitFromR r
 setValueToL l val1
executeInstruction (Krz16i r l) = do {- load highest 16bit from r; sign-extend; write to l -}
 val1 <- signExtendFrom16 <$> getHighest16bitFromR r
 setValueToL l val1
executeInstruction (Krz8c r l) = do {- load from r; truncate to 8bit; write to highest 8bit of l -}
 val1 <- truncateTo8 <$> getValueFromR r
 setValueToHighest8bitOfL l val1 
executeInstruction (Krz16c r l) = do {- load from r; truncate to 16bit; write to highest 16bit of l -}
 val1 <- truncateTo16 <$> getValueFromR r
 setValueToHighest16bitOfL l val1 

truncateTo8 :: Word32 -> Word8
truncateTo8 = fromIntegral

truncateTo16 :: Word32 -> Word16
truncateTo16 = fromIntegral

templ :: (Word32 -> Word32 -> Word32) -> Rvalue -> Lvalue -> VIO ()
templ func r l = do 
 val1 <- getValueFromR r
 val2 <- getValueFromR (L l)
 setValueToL l (func val2 val1)

liftMemOp :: State Memory a -> VIO a
liftMemOp memOp = do
 (cpu, mem) <- get
 let (a, newMem) = memOp `runState` mem
 put (cpu, newMem)
 return a

signExtendFrom8 :: Word8 -> Word32
signExtendFrom8 =
 (fromIntegral :: Int32 -> Word32) .
 (fromIntegral :: Int8 -> Int32) .
 (fromIntegral :: Word8 -> Int8)

signExtendFrom16 :: Word16 -> Word32
signExtendFrom16 =
 (fromIntegral :: Int32 -> Word32) .
 (fromIntegral :: Int16 -> Int32) .
 (fromIntegral :: Word16 -> Int16)


getHighest8bit :: Word32 -> Word8
getHighest8bit a = fromIntegral $ shiftR a (32 - 8)

getHighest16bit :: Word32 -> Word16
getHighest16bit a = fromIntegral $ shiftR a (32 - 16)

getHighest8bitFromR :: Rvalue -> VIO Word8
getHighest8bitFromR r@(Pure _) = getHighest8bit <$> getValueFromR r
getHighest8bitFromR r@(Lab _) = getHighest8bit <$> getValueFromR r
getHighest8bitFromR r@(L (Re _)) = getHighest8bit <$> getValueFromR r
getHighest8bitFromR (L (RPlusNum register offset)) = do
 v <- getRegister register
 liftMemOp $ readByte (v + offset)
getHighest8bitFromR (L (RPlusR r1 r2)) = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ readByte (v1 + v2)

getHighest16bitFromR :: Rvalue -> VIO Word16
getHighest16bitFromR r@(Pure _) = getHighest16bit <$> getValueFromR r
getHighest16bitFromR r@(Lab _) = getHighest16bit <$> getValueFromR r
getHighest16bitFromR r@(L (Re _)) = getHighest16bit <$> getValueFromR r
getHighest16bitFromR (L (RPlusNum register offset)) = do
 v <- getRegister register
 liftMemOp $ read16Bit (v + offset)
getHighest16bitFromR (L (RPlusR r1 r2)) = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ read16Bit (v1 + v2)

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

setValueToHighest8bitOfL :: Lvalue -> Word8 -> VIO ()
setValueToHighest8bitOfL (RPlusNum register offset) dat = do
 v <- getRegister register
 liftMemOp $ writeByte (v + offset) dat
setValueToHighest8bitOfL (RPlusR r1 r2) dat = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ writeByte (v1 + v2) dat
setValueToHighest8bitOfL (Re reg) dat = setHigh8bitOfRegister reg dat


setValueToHighest16bitOfL :: Lvalue -> Word16 -> VIO ()
setValueToHighest16bitOfL (RPlusNum register offset) dat = do
 v <- getRegister register
 liftMemOp $ write16Bit (v + offset) dat
setValueToHighest16bitOfL (RPlusR r1 r2) dat = do
 v1 <- getRegister r1
 v2 <- getRegister r2
 liftMemOp $ write16Bit (v1 + v2) dat
setValueToHighest16bitOfL (Re reg) dat = setHigh16bitOfRegister reg dat

getValueFromR :: Rvalue -> VIO Word32
getValueFromR (Pure word32) = return word32
getValueFromR (Lab label) = do
 program <- ask
 currentNX <- nx <$> getCPU
 case resolveLabel currentNX program label of
  Nothing -> runtimeError $ "Undefined label `" ++ unLabel label ++ "`"
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

ret :: Instruction
ret = Krz (L(RPlusNum F5 0)) (Re XX)

-- xx = nextAddressOf(nx); return getInstructionFrom(nx);
updateXXAndGetInstruction :: VIO Instruction
updateXXAndGetInstruction = do
 currentNX <- nx <$> getCPU
 program <- ask
 case readNX program currentNX of
  Nothing
   | currentNX == outermostRetAddress -> return TERMINATE
   | currentNX == debugOutputAddress -> do
    val <- getValueFromR (L (RPlusNum F5 4))
    tell [show val]
    return ret
   | otherwise -> runtimeError $ "nx has an invalid address " ++ show currentNX
  Just (newXX, instruction) -> do
   (cpu, mem) <- get
   put (cpu{xx = newXX}, mem)
   return instruction


initialF5 :: Word32
initialF5 = 0x6d7aa0f8

outermostRetAddress :: Word32
outermostRetAddress = 0xbda574b8

debugOutputAddress :: Word32
debugOutputAddress = 0xba5fb6b0

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
 