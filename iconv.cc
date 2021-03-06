/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include "iconv.hh"
#include <vector>
#include <cerrno>
#include <cstring>

char const * const Iconv::GdWchar = "WCHAR_T";
char const * const Iconv::Utf16Le = "UTF-16LE";
char const * const Iconv::Utf8 = "UTF-8";

using gd::wchar;

#define ICONV_ERROR reinterpret_cast<iconv_t>(-1)
#define ICONV_ERR static_cast<unsigned long>(-1)

Iconv::Iconv( char const * to, char const * from ):
    state( iconv_open( to, from ) )
{
    if ( state == ICONV_ERROR )
        throw exCantInit( strerror( errno ) );
}

void Iconv::reinit( char const * to, char const * from )
{
    iconv_close( state );

    state = iconv_open( to, from );

    if ( state == ICONV_ERROR )
        throw exCantInit( strerror( errno ) );
}

Iconv::~Iconv()
{
    iconv_close( state );
}

Iconv::Result Iconv::convert( void const * & inBuf, size_t  & inBytesLeft,
                              void * & outBuf, size_t & outBytesLeft )
{
    size_t result = iconv( state,
                           reinterpret_cast<char **>(const_cast<void **>(&inBuf)),
                           &inBytesLeft,
                           reinterpret_cast<char **>(&outBuf), &outBytesLeft );

    if ( result == ICONV_ERR )
    {
        switch( errno )
        {
            case EILSEQ:
                throw exIncorrectSeq();
            case EINVAL:
                return NeedMoreIn;
            case E2BIG:
                return NeedMoreOut;
            default:
                throw exOther( strerror( errno ) );
        }
    }

    return Success;
}

gd::wstring Iconv::toWstring( char const * fromEncoding, void const * fromData,
                              size_t dataSize )
{
    /// Special-case the dataSize == 0 to avoid any kind of iconv-specific
    /// behaviour in that regard.

    if ( !dataSize )
        return gd::wstring();

    Iconv ic( GdWchar, fromEncoding );

    /// This size is usually enough, but may be enlarged during the conversion
    std::vector< wchar > outBuf( dataSize );

    void * outBufPtr = &outBuf.front();

    size_t outBufLeft = outBuf.size() * sizeof( wchar );

    for( ; ; )
    {
        switch( ic.convert( fromData, dataSize, outBufPtr, outBufLeft ) )
        {
            case Success:
                return gd::wstring( &outBuf.front(),
                                    outBuf.size() - outBufLeft / sizeof( wchar ) );
            case NeedMoreIn:
                throw exPrematureEnd();
            case NeedMoreOut:
            {
                // Grow the buffer and retry
                // The pointer may get invalidated so we save the diff and restore it
                size_t offset = static_cast<wchar *>(outBufPtr) - &outBuf.front();
                outBuf.resize( outBuf.size() + 256 );
                outBufPtr = &outBuf.front() + offset;
                outBufLeft += 256;
            }
        }
    }
}

std::string Iconv::toUtf8( char const * fromEncoding, void const * fromData,
                           size_t dataSize )
{
    // Similar to toWstring

    if ( !dataSize )
        return std::string();

    Iconv ic( Utf8, fromEncoding );

    std::vector< char > outBuf( dataSize );

    void * outBufPtr = &outBuf.front();

    size_t outBufLeft = outBuf.size();

    for( ; ; )
    {
        switch( ic.convert( fromData, dataSize, outBufPtr, outBufLeft ) )
        {
            case Success:
                return std::string( &outBuf.front(),
                                    outBuf.size() - outBufLeft );
            case NeedMoreIn:
                throw exPrematureEnd();
            case NeedMoreOut:
            {
                // Grow the buffer and retry
                // The pointer may get invalidated so we save the diff and restore it
                size_t offset = static_cast<char *>(outBufPtr) - &outBuf.front();
                outBuf.resize( outBuf.size() + 256 );
                outBufPtr = &outBuf.front() + offset;
                outBufLeft += 256;
            }
        }
    }
}

