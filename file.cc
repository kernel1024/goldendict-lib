/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include "file.hh"

#include <cstring>
#include <cerrno>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

namespace File {

enum
{
    // We employ a writing buffer to considerably speed up file operations when
    // they consists of many small writes. The default size for the buffer is 64k
    WriteBufferSize = 65536
};

bool exists( char const * filename )
{
    struct stat buf {};

    // EOVERFLOW rationale: if the file is too large, it still does exist
    return stat( filename, &buf ) == 0 || errno == EOVERFLOW;
}

void Class::open( char const * filename, char const * mode )
{
    f = fopen( filename, mode );

    if ( !f )
        throw exCantOpen( std::string( filename ) + ": " + strerror( errno ) );
}

Class::Class( char const * filename, char const * mode ):
    f( nullptr ), writeBuffer( nullptr ), writeBufferLeft( 0 )
{
    open( filename, mode );
}

Class::Class( std::string const & filename, char const * mode ):
    f( nullptr ), writeBuffer( nullptr ), writeBufferLeft( 0 )
{
    open( filename.c_str(), mode );
}

void Class::read( void * buf, size_t size )
{
    if ( !size )
        return;

    if ( writeBuffer )
        flushWriteBuffer();

    size_t result = fread( buf, size, 1, f );

    if ( result != 1 )
        throw exReadError();
}

size_t Class::readRecords( void * buf, size_t size, size_t count )
{
    if ( writeBuffer )
        flushWriteBuffer();

    return fread( buf, size, count, f );
}

void Class::write( void const * buf, size_t size )
{
    if ( !size )
        return;

    if ( size >= WriteBufferSize )
    {
        // If the write is large, there's not much point in buffering
        flushWriteBuffer();

        size_t result = fwrite( buf, size, 1, f );

        if ( result != 1 )
            throw exWriteError();

        return;
    }

    if ( !writeBuffer )
    {
        // Allocate the writing buffer since we don't have any yet
        writeBuffer = new char[ WriteBufferSize ];
        writeBufferLeft = WriteBufferSize;
    }

    size_t toAdd = size < writeBufferLeft ? size : writeBufferLeft;

    memcpy( writeBuffer + ( WriteBufferSize - writeBufferLeft ),
            buf, toAdd );

    size -= toAdd;
    writeBufferLeft -= toAdd;

    if ( !writeBufferLeft ) // Out of buffer? Flush it.
    {
        flushWriteBuffer();

        if ( size ) // Something's still left? Add to buffer.
        {
            memcpy( writeBuffer, (char const *)buf + toAdd, size );
            writeBufferLeft -= size;
        }
    }
}

size_t Class::writeRecords( void const * buf, size_t size, size_t count )

{
    flushWriteBuffer();

    return fwrite( buf, size, count, f );
}

char * Class::gets( char * s, int size, bool stripNl )

{
    if ( writeBuffer )
        flushWriteBuffer();

    char * result = fgets( s, size, f );

    if ( result && stripNl )
    {
        size_t len = strlen( result );

        char * last = result + len;

        while( len-- )
        {
            --last;

            if ( *last == '\n' || *last == '\r' )
                *last = 0;
            else
                break;
        }
    }

    return result;
}

std::string Class::gets( bool stripNl )
{
    char buf[ 1024 ];

    if ( !gets( buf, sizeof( buf ), stripNl ) )
        throw exReadError();

    return std::string( buf );
}

void Class::seek( long offset )
{
    if ( writeBuffer )
        flushWriteBuffer();

    if ( fseek( f, offset, SEEK_SET ) != 0 )
        throw exSeekError();
}

void Class::seekCur( long offset )
{
    if ( writeBuffer )
        flushWriteBuffer();

    if ( fseek( f, offset, SEEK_CUR ) != 0 )
        throw exSeekError();
}

void Class::seekEnd( long offset )
{
    if ( writeBuffer )
        flushWriteBuffer();

    if ( fseek( f, offset, SEEK_END ) != 0 )
        throw exSeekError();
}

void Class::rewind()
{
    seek( 0 );
}

size_t Class::tell()
{
    long result = ftell( f );

    if ( result == -1 )
        throw exSeekError();

    if ( writeBuffer )
        result += ( WriteBufferSize - writeBufferLeft );

    return ( size_t ) result;
}

bool Class::eof()
{
    if ( writeBuffer )
        flushWriteBuffer();

    return feof( f );
}

FILE * Class::file()
{
    flushWriteBuffer();

    return f;
}

FILE * Class::release()
{
    releaseWriteBuffer();

    FILE * c = f;

    f = nullptr;

    return c;
}

void Class::close()
{
    fclose( release() );
}

Class::~Class()
{
    if ( f )
    {
        try
        {
            releaseWriteBuffer();
        }
        catch( exWriteError & )
        {
        }
        fclose( f );
    }
}

void Class::flushWriteBuffer()
{
    if ( writeBuffer && writeBufferLeft != WriteBufferSize )
    {
        size_t result = fwrite( writeBuffer, WriteBufferSize - writeBufferLeft, 1, f );

        if ( result != 1 )
            throw exWriteError();

        writeBufferLeft = WriteBufferSize;
    }
}

void Class::releaseWriteBuffer()
{
    flushWriteBuffer();

    if ( writeBuffer )
    {
        delete [] writeBuffer;

        writeBuffer = nullptr;
    }
}


}
