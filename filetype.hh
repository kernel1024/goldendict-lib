/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#ifndef __FILETYPE_HH_INCLUDED__
#define __FILETYPE_HH_INCLUDED__

#include <string>

/// Utilities to guess file types based on their names.
namespace Filetype {

using std::string;

/// Returns true if the name resembles the one of a picture file (i.e. ends
/// with .jpg, .png and such).
bool isNameOfPicture( string const & );

}

#endif

