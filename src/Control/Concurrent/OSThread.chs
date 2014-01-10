-- |
-- Copyright: (C) 2013 Amgen, Inc.
--
-- Needed definitions copied from @rts/[posix|win32]/OSThreads.c@ in GHC source.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Control.Concurrent.OSThread
  ( OSThreadId
  , myOSThreadId
  ) where

import Foreign.C

#ifdef H_ARCH_UNIX

#include <pthread.h>

type OSThreadId = {#type pthread_t #}

{# fun pthread_self as myOSThreadId {} -> `OSThreadId' id #}

#else

#include <Windows.h>

type OSThreadId = {#type DWORD #}

{# fun GetCurrentThreadId as myOSThreadId {} -> `OSThreadId' id #}

#endif
