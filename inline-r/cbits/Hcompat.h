/* Copyright (C) 2013-2014 Amgen, Inc.
 *
 * Compatibility macros and definitions required to build on some
 * platforms.
 */

#ifndef H_COMPAT__H
#define H_COMPAT__H

#ifdef H_ARCH_UNIX_DARWIN

/* NOTE: c2hs-0.17.2 and earlier choke on certain OS X system headers:
 * https://github.com/haskell/c2hs/issues/85
 *
 * This file must be #included in every *.chs file which #includes any
 * system header, before all other #includes.
 */

#define __AVAILABILITY__
#define __OSX_AVAILABLE_STARTING(a,b)
#define __OSX_AVAILABLE_BUT_DEPRECATED(a,b,c,d)
#define __OSX_AVAILABLE_BUT_DEPRECATED_MSG(a,b,c,d,e)

#endif /* H_ARCH_UNIX_DARWIN */

#endif /* H_COMPAT__H */
