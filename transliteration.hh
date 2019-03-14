/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#ifndef __TRANSLITERATION_HH_INCLUDED__
#define __TRANSLITERATION_HH_INCLUDED__

#include "dictionary.hh"
#include <map>

namespace Transliteration {

using std::map;
using gd::wstring;
using std::string;
using std::vector;
  
class Table: public map< wstring, wstring >
{
  unsigned maxEntrySize;

public:

  Table(): maxEntrySize( 0 )
  {}

  unsigned getMaxEntrySize() const
  { return maxEntrySize; }
  
protected:

  /// Inserts new entry into index. from and to are UTF8-encoded strings.
  /// Also updates maxEntrySize.
  void ins( char const * from, char const * to );
};

/// This is a base dictionary class for simple transliteratons
class TransliterationDictionary: public Dictionary::Class
{
  string name;
  Table const & table;
  bool caseSensitive;
  
public:

  TransliterationDictionary( string const & id, string const & name,
                             Table const & table,
                             bool caseSensitive = true );

  virtual string getName();

  virtual map< Dictionary::Property, string > getProperties();
  
  virtual unsigned long getArticleCount();

  virtual unsigned long getWordCount();
  
  virtual vector< wstring > getAlternateWritings( wstring const & )
   ;

  virtual sptr< Dictionary::WordSearchRequest > findHeadwordsForSynonym( wstring const & );
  
  virtual sptr< Dictionary::WordSearchRequest > prefixMatch( wstring const &,
                                                             unsigned long );
  
  virtual sptr< Dictionary::DataRequest > getArticle( wstring const &,
                                                      vector< wstring > const &,
                                                      wstring const & );
};

}

#endif
