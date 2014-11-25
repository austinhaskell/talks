{-# LANGUAGE ForeignFunctionInterface #-}
module Demo where

import Foreign
import Foreign.C
import Foreign.Marshal.Alloc

data Day = Day { month :: CUInt, day :: CUInt, year :: CUInt }
   deriving (Show)

data Student = Student { name :: String, birthday :: Day }
   deriving (Show)

instance Storable Day where
  sizeOf _    = 12
  alignment _ = 4

  peek ptr = do
     m <- peekByteOff ptr 0
     d <- peekByteOff ptr 4
     y <- peekByteOff ptr 8
     return (Day m d y)

  poke ptr (Day m d y) = do
     pokeByteOff ptr 0 m
     pokeByteOff ptr 4 d
     pokeByteOff ptr 8 y

instance Storable Student where
  sizeOf _    = 24
  alignment _ = 8

  peek ptr = do
     np <- peekByteOff ptr 0
     n <- peekCString np
     d <- peekByteOff ptr 8
     return (Student n d)

  poke ptr (Student n d) = withCString n $ \nameptr -> do
    pokeByteOff ptr 0 nameptr
    pokeByteOff ptr 8 d

foreign import ccall "get_student"
   c_get_student :: CUInt -> IO (Ptr Student)

foreign import ccall "update_student"
   c_update_student :: CUInt -> Ptr Student -> IO ()

get_student :: CUInt -> IO Student
get_student n = c_get_student n >>= peek

update_student :: CUInt -> Student -> IO ()
update_student n s = alloca $ \ptr -> poke ptr s >> c_update_student n ptr
