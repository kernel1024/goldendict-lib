/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include "indexedzip.hh"
#include "zipfile.hh"
extern "C" {
#include <zlib.h>
}
#include <QDebug>

using namespace BtreeIndexing;
using std::vector;

bool IndexedZip::openZipFile( QString const & name )
{
    zip.setFileName( name );

    zipIsOpen = zip.open( QFile::ReadOnly );

    return zipIsOpen;
}

bool IndexedZip::hasFile( gd::wstring const & name )
{
    if ( !zipIsOpen )
        return false;

    vector< WordArticleLink > links = findArticles( name );

    return !links.empty();
}

bool IndexedZip::loadFile( gd::wstring const & name, vector< char > & data )
{
    if ( !zipIsOpen )
        return false;

    vector< WordArticleLink > links = findArticles( name );

    if ( links.empty() )
        return false;

    // Now seek into the zip file and read its header

    if ( !zip.seek( links[ 0 ].articleOffset ) )
        return false;

    ZipFile::LocalFileHeader header;

    if ( !ZipFile::readLocalHeader( zip, header ) )
    {
        qWarning() << "Failed to load header";
        return false;
    }

    // Which algorithm was used?

    switch( header.compressionMethod )
    {
        case ZipFile::Uncompressed:
            //printf( "Uncompressed\n" );
            data.resize( header.uncompressedSize );
            return zip.read( &data.front(), data.size() ) == static_cast<qint64>(data.size());

        case ZipFile::Deflated:
        {
            //printf( "Deflated\n" );

            // Now do the deflation

            QByteArray compressedData = zip.read( header.compressedSize );

            if ( compressedData.size() != static_cast<int>(header.compressedSize) )
                return false;

            data.resize( header.uncompressedSize );

            z_stream stream;

            memset( &stream, 0, sizeof( stream ) );

            stream.next_in = reinterpret_cast< Bytef *>(compressedData.data());
            stream.avail_in = compressedData.size();
            stream.next_out = reinterpret_cast< Bytef *>(&data.front());
            stream.avail_out = data.size();

            if ( inflateInit2( &stream, -MAX_WBITS ) != Z_OK )
            {
                data.clear();
                return false;
            }

            if ( inflate( &stream, Z_FINISH ) != Z_STREAM_END )
            {
                qWarning() << "Not zstream end!";

                data.clear();

                inflateEnd( &stream );

                return false;
            }

            inflateEnd( &stream );

            return true;
        }

        default:

            return false;
    }
}
