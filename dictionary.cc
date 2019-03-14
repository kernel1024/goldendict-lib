/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include <vector>
#include <algorithm>
#include <cstdio>
#include "dictionary.hh"

#include <QCryptographicHash>

// For needToRebuildIndex(), read below
#include <QFileInfo>
#include <QDateTime>

//#include "config.hh"
#include <QDir>
#include <QFileInfo>
#include "fsencoding.hh"

namespace Dictionary {

bool Request::isFinished()
{
  return (isFinishedFlag.load()!=0);
}

void Request::update()
{
  if ( isFinishedFlag.load() == 0 )
    emit updated();
}

void Request::finish()
{
  if ( isFinishedFlag.load() == 0 )
  {
    isFinishedFlag.ref();

    emit finished();
  }
}

void Request::setErrorString( QString const & str )
{
  Mutex::Lock _( errorStringMutex );

  errorString = str;
}

QString Request::getErrorString()
{
  Mutex::Lock _( errorStringMutex );

  return errorString;
}


///////// WordSearchRequest
  
size_t WordSearchRequest::matchesCount()
{
  Mutex::Lock _( dataMutex );
  
  return matches.size();
}

WordMatch WordSearchRequest::operator [] ( size_t index )
{
  Mutex::Lock _( dataMutex );
  
  if ( index >= matches.size() )
    throw exIndexOutOfRange();
  
  return matches[ index ];
}

vector< WordMatch > & WordSearchRequest::getAllMatches()
{
  if ( !isFinished() )
    throw exRequestUnfinished();

  return matches;
}

////////////// DataRequest

long DataRequest::dataSize()
{
  Mutex::Lock _( dataMutex );
  
  return hasAnyData ? (long)data.size() : -1;
}

void DataRequest::getDataSlice( size_t offset, size_t size, void * buffer )
{
  Mutex::Lock _( dataMutex );

  if ( offset + size > data.size() || !hasAnyData )
    throw exSliceOutOfRange();

  memcpy( buffer, &data[ offset ], size );
}

vector< char > & DataRequest::getFullData()
{
  if ( !isFinished() )
    throw exRequestUnfinished();

  return data;
}

Class::Class( string const & id_, vector< string > const & dictionaryFiles_ ):
  id( id_ ), dictionaryFiles( dictionaryFiles_ )
{
}

void Class::deferredInit()
{
}

sptr< WordSearchRequest > Class::stemmedMatch( wstring const & /*str*/,
                                               unsigned /*minLength*/,
                                               unsigned /*maxSuffixVariation*/,
                                               unsigned long /*maxResults*/ )
{
  return new WordSearchRequestInstant();
}

sptr< WordSearchRequest > Class::findHeadwordsForSynonym( wstring const & )
{
  return new WordSearchRequestInstant();
}

vector< wstring > Class::getAlternateWritings( wstring const & )
{
  return vector< wstring >();
}

sptr< DataRequest > Class::getResource( string const & /*name*/ )
{
  return new DataRequestInstant( false );
}


string makeDictionaryId( vector< string > const & dictionaryFiles )
{
  std::vector< string > sortedList;

  sortedList = dictionaryFiles;

  std::sort( sortedList.begin(), sortedList.end() );

  QCryptographicHash hash( QCryptographicHash::Md5 );

  for( std::vector< string >::const_iterator i = sortedList.begin();
       i != sortedList.end(); ++i )
    hash.addData( i->c_str(), i->size() + 1 );

  return hash.result().toHex().constData();
}

// While this file is not supposed to have any Qt stuff since it's used by
// the dictionary backends, there's no platform-independent way to get hold
// of a timestamp of the file, so we use here Qt anyway. It is supposed to
// be fixed in the future when it's needed.
bool needToRebuildIndex( vector< string > const & dictionaryFiles,
                         string const & indexFile )
{
  qint64 lastModified = 0;

  for( const auto & dict : dictionaryFiles )
  {
    QString name = FsEncoding::decode( dict.c_str() );
    QFileInfo fileInfo( name );

    if ( fileInfo.isDir() )
      continue;

    if ( !fileInfo.exists() )
      return true;

    qint64 ts = fileInfo.lastModified().toSecsSinceEpoch();

    if ( ts > lastModified )
      lastModified = ts;
  }

  QFileInfo fileInfo( FsEncoding::decode( indexFile.c_str() ) );

  if ( !fileInfo.exists() )
    return true;

  return fileInfo.lastModified().toSecsSinceEpoch() < lastModified;
}

}
