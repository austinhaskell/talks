% What the FFI?!
% Josh Cartwright; Austin Haskell Users Group
% November 24, 2014

# Foreign Function Interface

> ... its aim is to support the implementation of programs in a mixture of
> Haskell and other languages such that the source code is portable across
> different implementations of Haskell and non-Haskell systems as well as
> independent of the architecture and operating system.

<div class="notes">
We all know that Haskell will take over the world.  Static types will solve all
of the worlds problems.  In a few years, we'll be in a Haskell-driven utopia.

Unfortunately, until we get there, Haskell exists as part of a larger software
ecosystem.  In order for Haskell to be most useful in this environment, it
needs to be adapted to allow intercall/interoperability with code not written
in Haskell ("Foreign") code.

The Foreign Function Interface (FFI) is a specification/extension of Haskell
syntax that allows for describing an interface to this "Foreign" code.  It came
into existance as a GHC extension to allow for calling C code, but has since been
part of the Haskell 2010 report, and adopted with other Haskell compilers to
interact with languages other than C.

Or, in GIF form:
</div>

#

![teamwork](images/teamwork.gif)\


# Goal: implement a simple C binding

* Assumption: already familiar with Haskell syntax
* Assumption: already familiar with Haskell typeclasses

<div class="notes">
The FFI is a big topic, one that I could not possibly describe in it's entirety
during this presentation.  Instead, I'll be focusing attention on one "vertical
slice", calling a C function from haskell.  This will allow me to cover the
"big" pieces, if your interested in learning more, see the references on the
last slide.

With this in mind, the hope is that I can explain how to do this, even if you
don't necessarily have a background in C.  With that, let's cover at least some
C basics:
</div>

# C: Primitive types

* `char`{.c}
* `int`{.c}
* `short int`{.c}
* `long int`{.c}
* `long long int`{.c}
* `float`{.c}
* `double`{.c}

<div class="notes">
C defines a handful of primitive types.  char, int, short int, long int, and
long long int, are integral types, they represent integers.  Their types can be
modified with the keyword "unsigned", meaning that the values are all >0.
</div>

# C: Pointers

![don't touch](images/donttouch.gif)\


# C: Pointers

````c
int *example1;

double *example2;

double **example3;
````

<div class="notes">
In addition to primitive types, C supports the concept of a "pointer".  I won't
be going into too many details about pointers, but I will state that a pointer
represents an address in memory, which points to something with a specific type.
</div>

# C: Strings

````c
char *example4;
````

<div class="notes">
In C, strings are are represented as a series of single byte characters, followed
by a null byte.

What I intend to demonstrate here is that example4, while being declared as a
"pointer to char", it may in fact point to an array of characters.  This will
become important later.
</div>

# C: `struct`{.c}

````c
struct day {
   unsigned int month;
   unsigned int day;
   unsigned int year;
};

struct student {
   char       *name;
   struct day birthday;
};

struct student john = {
   .name     = "John Smith",
   .birthday =  {
      .month = 11,
      .day   = 24,
      .year  = 1972,
   },
};
````

<div class="notes">
From the primitive types and pointers, you can construct more complicated
types, such as these two, 'struct day' and 'struct student'.  A 'struct' is
effectively a poor man's product type.  An object of type 'struct day' is a
tuple with members 'month' of type 'unsigned int' and 'day' also of type
'unsigned int'.

A struct may also contain another, as shown with 'struct student'.  In this case,
an object of type 'struct student' contains a pointer to a char and also
'birthday' which is of type 'struct day'.

Lastly, here's an example of the declaration of an object of struct student
named 'john'.
</div>

#

````c

struct day {
   unsigned int month;
   unsigned int day;
   unsigned int year;
};

struct student {
   char       *name;
   struct day birthday;
};

````

````haskell

import Foreign.C.Types

data Day = Day { month :: CUInt
               , day   :: CUInt
               , year  :: CUInt
               }
  deriving (Show)

data Student = Student { name :: String
                       , birthday :: Day
                       }
  deriving (Show)

````

<div class="notes">
The data types from the C API and the Haskell equivalent.
</div>

# Primitive type mapping

C                                                    Signed              Unsigned
--                            --                    -------             ---------
`char`{.c}                                          `CChar`{.haskell}    `CUChar`{.haskell}
`int`{.c}                                           `CInt`{.haskell}     `CUInt`{.haskell}
`short int`{.c}                                     `CShort`{.haskell}   `CUShort`{.haskell}
`long int`{.c}                                      `CLong`{.haskell}    `CULong`{.haskell}
`long long int`{.c}                                 `CLLong`{.haskell}   `CULLong`{.haskell}
`float`{.c}                                         `CFloat`{.haskell}   N/A
`double`{.c}                                        `CDouble`{.haskell}  N/A

<div class="notes">
As seen on the previous page, Foreign.C.Types defines a few types which
correspond to their C equivalents.  Here is a more complete table.
</div>

# C: Functions

````c
struct student *get_student(unsigned int id);

void update_student(unsigned int id, struct student *s);
````

<div class="notes">
Along with types, C also has functions, and they are declared as follows.

The first function is named 'get_student', and accepts as an argument an
unsigned int.  It returns a pointer to an object of type 'struct student'.

In the second case, a function named 'update_student' accepts an unsigned
integer and a pointer to an object of type 'struct student' and returns nothing
(void).
</div>

#

````c

struct student *get_student(unsigned int id);

void update_student(unsigned int id, struct student *s);

````

````haskell
{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "get_student"
   c_get_student :: CUInt -> IO (Ptr Student)

foreign import ccall "update_student"
   c_update_student :: CUInt -> Ptr Student -> IO ()

````

<div class="notes">
And for the function mappings.

Here we see for the first time a few keywords, these keywords are available when
you've enabled the ForeignFunctionInterface language extension:
  - foreign : indicates that this function is visible externally to haskell
  - import : indicates that this function is being imported, that is, it's being used by Haskell and is defined in C
  - ccall: indicates the calling convention used to call this function

In addition, there is a `String`{.haskell} argument which is used to give the
corresponding name known to C, the name of this identifier in Haskell, and the
function type.

So, we've got these Ptr Students floating around, how are we expected to make
use of them?
</div>

#

````haskell
class Storable a where
   sizeOf :: a -> Int

   alignment :: a -> Int

   peek :: Ptr a -> IO a

   poke :: Ptr a -> a -> IO ()
````

<div class="notes">
A type which implements Storable means that it's capable of being converted
to/from an unstructured memory buffer.

This typeclass is defined in Foreign.Storable
</div>

#

````haskell

data Day = Day { month :: CUInt, day :: CUInt, year :: CUInt }
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

peekByteOff :: Storable a => Ptr b -> Int -> IO a
pokeByteOff :: Storable a => Ptr b -> Int -> a -> IO ()
````

#

````haskell

data Student = Student { name :: String, birthday :: Day }
   deriving (Show)

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

withCString :: String -> (CString -> IO a) -> IO a
````

_NOTE_: This usage of withCString is _wrong_!  We end up writing
a pointer to the 'struct student' object which becomes invalid/potentially
freed after the poke!  The correct way to do this might be to use ForeignPtr.
(This note added after the presentation given).

# Demo

#

````haskell
get_student :: CUInt -> IO Student
get_student n = c_get_student n >>= peek
````

#

````haskell
c_update_student :: CUInt -> Ptr Student -> IO ()

update_student :: CUInt -> Student -> IO ()
````

<div class="notes">
Recall the type of c_update_student.  Just like the get_student example, we'll
want to create a higher-level update_student API which makes things a bit easier.
But, how would we implement this?  We need to somehow get an instance of Ptr
Student...
</div>

#

````haskell

import Foreign.Marshal.Alloc (alloca)

alloca :: Storable a => (Ptr a -> IO b) -> IO b
````

<div class="notes">
How do we use this thing?
</div>

#

````haskell

import Foreign.Marshal.Alloc (alloca)

update_student :: CUInt -> Student -> IO ()
update_student n s = alloca $ \ptr -> do
   poke ptr s
   c_update_student n ptr
````

<div class="notes">
Here, we are creating a update_student function which a much
more convenient type to work with.  Under the hood we make use of
alloca to get a handle on a buffer large enough to hold a Student
object.

We then make use of the Student Storable instance to write a Student
object into this buffer and call the imported c_update_student.
</div>

# Demo

# The End. Thanks!
@joshcartwright

joshc on freenode

\#haskell.austin

# References

* [Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/)
* [FFI Introducion on Haskellwiki](https://www.haskell.org/haskellwiki/FFI_Introduction)
* [RWH chapter on FFI](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)
* [ABI on wikipedia](http://en.wikipedia.org/wiki/Application_binary_interface)
* [hsc2hs manual](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/hsc2hs.html)
