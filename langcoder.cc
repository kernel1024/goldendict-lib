#include "langcoder.hh"
#include "folding.hh"
#include "wstring_qt.hh"

#include <cctype>

LangCoder langCoder;


LangCoder::LangCoder()
{
  LangStruct ls;
  for (int i = 0; true; i++) {
    const LangCode &lc = LangCodes[i];
    if (lc.lang[0] == 0)
      break;
    codeMap[code2toInt(lc.code)] = i;
  }
}

QString LangCoder::decode(quint32 code)
{
  if (langCoder.codeMap.contains(code))
    return LangCodes[langCoder.codeMap[code]].lang;

  return QString();
}

LangStruct LangCoder::langStruct(quint32 code)
{
  LangStruct ls;
  ls.code = code;
  ls.order = -1;
  if (codeMap.contains(code)) {
    int order = codeMap[code];
    const LangCode &lc = LangCodes[order];
    ls.order = order;
    ls.lang = lc.lang;
  }
  return ls;
}

QString LangCoder::intToCode2( quint32 val )
{
  if ( !val || val == 0xFFffFFff )
    return QString();

  char code[ 2 ];

  code[ 0 ] = val & 0xFF;
  code[ 1 ] = ( val >> 8 ) & 0xFF;

  return QString::fromLatin1( code, 2 );
}

quint32 LangCoder::code3toInt(const std::string& code3)
{
  if (code3.length() < 2)
    return 0;

  // this is temporary
  char code1 = tolower( code3.at(1) );
  char code0 = tolower( code3.at(0) );

  return ( (static_cast<quint32>(code1)) << 8 ) + static_cast<quint32>(code0);
}

quint32 LangCoder::findIdForLanguage( gd::wstring const & lang )
{
  gd::wstring langFolded = Folding::apply( lang );

  for( LangCode const * lc = LangCodes; lc->code[ 0 ]; ++lc )
  {
    if ( langFolded == Folding::apply( gd::toWString( lc->lang ) ) )
    {
      // We've got a match
      return code2toInt( lc->code );
    }
  }

  return 0;
}

quint32 LangCoder::guessId( const QString & lang )
{
  QString lstr = lang.simplified().toLower();

  // too small to guess
  if (lstr.size() < 2)
    return 0;

  // check if it could be the whole language name
  if (lstr.size() >= 3)
  {
    for( LangCode const * lc = LangCodes; lc->code[ 0 ]; ++lc )
    {
      if ( lstr == QString( lc->lang ) )
      {
        // We've got a match
        return code2toInt( lc->code );
      }
    }
  }

  // still not found - try to match by 2-symbol code
  return code2toInt( lstr.left(2).toLatin1().constData() );
}

QPair<quint32,quint32> LangCoder::findIdsForFilename( QString const & name )
{
  QString nameFolded = "|" + QFileInfo( name ).fileName().toCaseFolded() + "|";

  QRegExp reg( "[^a-z]([a-z]{2,3})-([a-z]{2,3})[^a-z]" ); reg.setMinimal(true);
  int off = 0;
  while ( reg.indexIn( nameFolded, off ) >= 0 )
  {

    quint32 from = guessId( reg.cap(1) );
    quint32 to = guessId( reg.cap(2) );
    if (from && to)
      return QPair<quint32,quint32>(from, to);

    off += reg.matchedLength();
  }

  return QPair<quint32,quint32>(0, 0);
}
