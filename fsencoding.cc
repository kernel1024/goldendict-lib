/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include "fsencoding.hh"
#include "wstring_qt.hh"
#include <QString>
#include <QDir>
#include <vector>

namespace FsEncoding {

string encode( wstring const & str )
{
    return string( gd::toQString( str ).toLocal8Bit().constData() );
}

string encode( string const & str )
{
    return string( QString::fromUtf8( str.c_str() ).toLocal8Bit().constData() );
}

string encode( QString const & str )
{
    return string( str.toLocal8Bit().constData() );
}

wstring decode( string const & str )
{
    return gd::toWString( QString::fromLocal8Bit( str.c_str() ) );
}

QString decode( const char *str )
{
    return QString::fromLocal8Bit( str );
}

char separator()
{
    return QDir::separator().toLatin1();
}

string dirname( string const & str )
{
    size_t x = str.rfind( separator() );

    if ( x == string::npos )
        return string( "." );

    return string( str, 0, x );
}

string basename( string const & str )
{
    size_t x = str.rfind( separator() );

    if ( x == string::npos )
        return str;

    return string( str, x + 1 );
}

}
