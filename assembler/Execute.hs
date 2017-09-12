module Execute
(initialCPU
) where
import Types
import Memory
import TentativeLoad

initialF5 :: Word32
initialF5 = 0x6d7aa0f8

outermostRetAddress :: Word32
outermostRetAddress = 0xbda574b8

initialMemory :: Memory
initialMemory = writeM initialF5 outermostRetAddress `execState` emptyM

initialCPU :: Word32 -> Word32 -> CPU
initialCPU initNX initXX = CPU{
 f0 = 0x82ebfc85, -- garbage
 f1 = 0xfc73c497, -- garbage
 f2 = 0x9cf84b9d, -- garbage
 f3 = 0x92c073e6, -- garbage
 f5 = initialF5,
 flag = False,
 memory = initialMemory,
 nx = initNX,
 xx = initXX
 }
 