/* Lightweight framework for goldendict created by <kernelonline@gmail.com>
 *
 * Partials from project GoldenDict.
 *
 * (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include <vector>
#include <string>
#include <set>
#include <list>
#include "globalcontrol.h"
#include "goldendictmgr.h"
#include "dsl.hh"
#include "stardict.hh"
#include "dictdfiles.hh"
#include "wstring_qt.hh"
#include "htmlescape.hh"
#include "langcoder.hh"
#include "folding.hh"
#include "utf8.hh"
#include "romaji.hh"
#include "mainwindow.h"

#include <QMessageBox>
#include <QString>
#include <QUrl>
#include <QDebug>

#include <QUrlQuery>

using std::vector;
using std::string;
using gd::wstring;
using std::set;
using std::list;

namespace
{
  /// Uses some heuristics to chop off the first domain name from the host name,
  /// but only if it's not too base. Returns the resulting host name.
  QString getHostBase( QUrl const & url )
  {
    QString host = url.host();

    QStringList domains = host.split( '.' );

    int left = domains.size();

    // Skip last <=3-letter domain name
    if ( left && domains[ left - 1 ].size() <= 3 )
      --left;

    // Skip another <=3-letter domain name
    if ( left && domains[ left - 1 ].size() <= 3 )
      --left;

    if ( left > 1 )
    {
      // We've got something like www.foobar.co.uk -- we can chop off the first
      // domain

      return host.mid( domains[ 0 ].size() + 1 );
    }
    else
      return host;
  }
}

CGoldenDictMgr::CGoldenDictMgr(QObject *parent) :
    QObject(parent)
{
}

sptr<Dictionary::DataRequest> CGoldenDictMgr::makeDefinitionFor(const QString &inWord, const QMap<QString, QString> &contexts) const
{
    // Find the given group

    string header = makeHtmlHeader( inWord.trimmed() );

    return new ArticleRequest( inWord.trimmed(), "", contexts, dictionaries, header );
}

sptr<Dictionary::DataRequest> CGoldenDictMgr::makeNotFoundTextFor(const QString &word) const
{
    string result = makeHtmlHeader( word ) + makeNotFoundBody( word ) +
      "</body></html>";

    sptr< Dictionary::DataRequestInstant > r = new Dictionary::DataRequestInstant( true );

    r->getData().resize( result.size() );
    memcpy( &( r->getData().front() ), result.data(), result.size() );

    return r;
}

sptr<Dictionary::DataRequest> CGoldenDictMgr::makeEmptyPage() const
{
    string result = makeHtmlHeader( tr( "(untitled)" ) ) +
      "</body></html>";

    sptr< Dictionary::DataRequestInstant > r =
        new Dictionary::DataRequestInstant( true );

    r->getData().resize( result.size() );
    memcpy( &( r->getData().front() ), result.data(), result.size() );

    return r;
}

QStringList CGoldenDictMgr::getLoadedDictionaries()
{
    QStringList res;
    res.clear();

    for( unsigned x = dictionaries.size(); x--; )
        res << tr("%1 (%2 words)")
               .arg(QString::fromUtf8(dictionaries[x]->getName().c_str()),
               dictionaries[x]->getWordCount());

    return res;
}


std::string CGoldenDictMgr::makeNotFoundBody(const QString &word)
{
    string result( "<div class=\"gdnotfound\"><p>" );

    if ( word.size() )
      result += tr( "No translation for <b>%1</b> was found." ).
                arg( QString::fromUtf8( Html::escape( word.toUtf8().data() ).c_str() ) ).
                toUtf8().data();
    else
      result += tr( "No translation was found." ).toUtf8().data();

    result += "</p></div>";

    return result;
}


std::string CGoldenDictMgr::makeHtmlHeader(const QString &word) const
{
    string result =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
      "<html><head>"
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">";

    // Add a css stylesheet

    {
      QFile builtInCssFile( ":/data/article-style.css" );
      builtInCssFile.open( QFile::ReadOnly );
      QByteArray css = builtInCssFile.readAll();

      result += "<style type=\"text/css\" media=\"all\">\n";
      result += css.data();
      result += "</style>\n";
    }

    result += "<title>" + Html::escape( Utf8::encode( gd::toWString( word ) ) ) + "</title>";

    // This doesn't seem to be much of influence right now, but we'll keep
    // it anyway.

    result += "</head><body>";

    return result;

}

void CGoldenDictMgr::loadDictionaries()
{
    dictionaries.clear();

    showMessage(tr("Loading dictionaries..."));

    qInfo() << "Indexing dictionaries. Please wait.";

    CDictLoader *loadDicts = new CDictLoader(this, gSet->dictPaths);

    QObject::connect( loadDicts, SIGNAL( indexingDictionarySignal( QString const & ) ),
                      this, SLOT(showMessage(QString)) );

    QObject::connect( loadDicts, SIGNAL( finished() ),
                      this, SLOT( loadDone() ));

    loadDicts->start();
}

void CGoldenDictMgr::loadDone()
{
    CDictLoader* loadDicts = qobject_cast<CDictLoader *>(sender());

    if (loadDicts==NULL) {
        qCritical() << "Unexpected call for loadDone.";
        showMessage(tr("ERROR: Unexpected call for loadDone. Dictionaries unavailable!"));
        return;
    }

    loadDicts->wait();

    showMessage(QString());


    if ( loadDicts->getExceptionText().size() )
    {
      QMessageBox::critical( gSet->activeWindow, tr("JPReader"),
                             tr("Error loading dictionaries %1").
                             arg(QString::fromUtf8( loadDicts->getExceptionText().c_str() ) ));

      return;
    }

    dictionaries = loadDicts->getDictionaries();

    // Make Romaji
    vector< sptr< Dictionary::Class > > romajiDictionaries = Romaji::makeDictionaries();

    dictionaries.insert( dictionaries.end(), romajiDictionaries.begin(),
                         romajiDictionaries.end() );

    qInfo() << "Dictionaries loaded";

    // Remove any stale index files

    std::set< std::string > ids;

    for( unsigned x = dictionaries.size(); x--; )
      ids.insert( dictionaries[ x ]->getId() );

    QDir indexDir( gSet->dictIndexDir );

    QStringList allIdxFiles = indexDir.entryList( QDir::Files );

    for( QStringList::const_iterator i = allIdxFiles.constBegin();
         i != allIdxFiles.constEnd(); ++i )
    {
      if ( ids.find( i->toLocal8Bit().data() ) == ids.end() &&
           i->size() == 32 )
        indexDir.remove( *i );
    }

    loadDicts->deleteLater();
}

void CGoldenDictMgr::showMessage(const QString &msg)
{
    if (gSet->activeWindow!=NULL) {
        if (msg.isEmpty())
            gSet->activeWindow->statusBar()->clearMessage();
        else
            gSet->activeWindow->statusBar()->showMessage(msg);
    }
}


CDictLoader::CDictLoader(QObject *parent, const QStringList &dictPaths)
    : QThread(parent), paths(dictPaths), exceptionText( "Load did not finish" )
{
    nameFilters << "*.ifo" << "*.dat"
                << "*.dsl" << "*.dsl.dz"  << "*.index";
}

void CDictLoader::run()
{
    try {
        for (int i=0;i<paths.count();i++)
            handlePath(paths.at(i),true);

        exceptionText.clear();
    }
    catch( std::exception & e )
    {
        exceptionText = e.what();
    }
}

void CDictLoader::indexingDictionary(const std::string &dictionaryName) throw()
{
    QString msg = tr("Indexing dictionary: %1").arg(QString::fromUtf8(dictionaryName.c_str()));
    emit indexingDictionarySignal( msg );
}

void CDictLoader::handlePath(const QString &path, bool recursive)
{
    std::vector< std::string > allFiles;

    QDir dir( path );

    QFileInfoList entries = dir.entryInfoList( nameFilters,
                                               QDir::AllDirs | QDir::Files | QDir::NoDotAndDotDot );

    for( QFileInfoList::const_iterator i = entries.constBegin();
         i != entries.constEnd(); ++i )
    {
      QString fullName = i->canonicalFilePath();

      if ( recursive && i->isDir() )
      {
        // Make sure the path doesn't look like with dsl resources
        if ( !fullName.endsWith( ".dsl.files", Qt::CaseInsensitive ) &&
             !fullName.endsWith( ".dsl.dz.files", Qt::CaseInsensitive ) )
          handlePath( fullName, true );
      }

      allFiles.push_back( QDir::toNativeSeparators( fullName ).toLocal8Bit().data() );
    }

    {
      std::vector< sptr< Dictionary::Class > > stardictDictionaries =
        Stardict::makeDictionaries( allFiles, gSet->dictIndexDir.toLocal8Bit().data(), *this );

      dictionaries.insert( dictionaries.end(), stardictDictionaries.begin(),
                           stardictDictionaries.end() );
    }

    {
      std::vector< sptr< Dictionary::Class > > dslDictionaries =
        Dsl::makeDictionaries( allFiles, gSet->dictIndexDir.toLocal8Bit().data(), *this );

      dictionaries.insert( dictionaries.end(), dslDictionaries.begin(),
                           dslDictionaries.end() );
    }

    {
      std::vector< sptr< Dictionary::Class > > dictdDictionaries =
        DictdFiles::makeDictionaries( allFiles, gSet->dictIndexDir.toLocal8Bit().data(), *this );

      dictionaries.insert( dictionaries.end(), dictdDictionaries.begin(),
                           dictdDictionaries.end() );
    }
}

//////// ArticleRequest

ArticleRequest::ArticleRequest(
  QString const & word_, QString const & group_,
  QMap< QString, QString > const & contexts_,
  vector< sptr< Dictionary::Class > > const & activeDicts_,
  string const & header ):
    word( word_ ), group( group_ ), contexts( contexts_ ),
    activeDicts( activeDicts_ ),
    altsDone( false ), bodyDone( false ), foundAnyDefinitions( false ),
    closePrevSpan( false )
{
  // No need to lock dataMutex on construction

  hasAnyData = true;

  data.resize( header.size() );
  memcpy( &data.front(), header.data(), header.size() );

  // Accumulate main forms

  for( unsigned x = 0; x < activeDicts.size(); ++x )
  {
    sptr< Dictionary::WordSearchRequest > s = activeDicts[ x ]->findHeadwordsForSynonym( gd::toWString( word ) );

    connect( s.get(), SIGNAL( finished() ),
             this, SLOT( altSearchFinished() ) );

    altSearches.push_back( s );
  }

  altSearchFinished(); // Handle any ones which have already finished
}

void ArticleRequest::altSearchFinished()
{
  if ( altsDone )
    return;

  // Check every request for finishing
  for( list< sptr< Dictionary::WordSearchRequest > >::iterator i =
         altSearches.begin(); i != altSearches.end(); )
  {
    if ( (*i)->isFinished() )
    {
      // This one's finished
      for( size_t count = (*i)->matchesCount(), x = 0; x < count; ++x )
        alts.insert( (**i)[ x ].word );

      altSearches.erase( i++ );
    }
    else
      ++i;
  }

  if ( altSearches.empty() )
  {
    //printf( "alts finished\n" );

    // They all've finished! Now we can look up bodies

    altsDone = true; // So any pending signals in queued mode won't mess us up

    vector< wstring > altsVector( alts.begin(), alts.end() );

    for( unsigned x = 0; x < altsVector.size(); ++x )
    {
      //printf( "Alt: %ls\n", altsVector[ x ].c_str() );
    }

    wstring wordStd = gd::toWString( word );

    for( unsigned x = 0; x < activeDicts.size(); ++x )
    {
      sptr< Dictionary::DataRequest > r =
        activeDicts[ x ]->getArticle( wordStd, altsVector,
                                      gd::toWString( contexts.value( QString::fromStdString( activeDicts[ x ]->getId() ) ) ) );

      connect( r.get(), SIGNAL( finished() ),
               this, SLOT( bodyFinished() ) );

      bodyRequests.push_back( r );
    }

    bodyFinished(); // Handle any ones which have already finished
  }
}

void ArticleRequest::bodyFinished()
{
  if ( bodyDone )
    return;

  //printf( "some body finished\n" );

  bool wasUpdated = false;

  while ( bodyRequests.size() )
  {
    // Since requests should go in order, check the first one first
    if ( bodyRequests.front()->isFinished() )
    {
      // Good

      //printf( "one finished.\n" );

      Dictionary::DataRequest & req = *bodyRequests.front();

      QString errorString = req.getErrorString();

      if ( req.dataSize() >= 0 || errorString.size() )
      {
        sptr< Dictionary::Class > const & activeDict =
            activeDicts[ activeDicts.size() - bodyRequests.size() ];

        string dictId = activeDict->getId();

        string head;

        string gdFrom = "gdfrom-" + Html::escape( dictId );

        if ( closePrevSpan )
        {
          head += "</span></span><span class=\"gdarticleseparator\"></span>";
        }
        else
        {
          // This is the first article
          head += "<script language=\"JavaScript\">"
                  "var gdCurrentArticle=\"" + gdFrom  + "\";</script>";
        }

        string jsVal = Html::escapeForJavaScript( dictId );
        head += "<script language=\"JavaScript\">var gdArticleContents; "
          "if ( !gdArticleContents ) gdArticleContents = \"" + jsVal +" \"; "
          "else gdArticleContents += \"" + jsVal + " \";</script>";

        head += string( "<span class=\"gdarticle" ) +
                ( closePrevSpan ? "" : " gdactivearticle" ) +
                "\" id=\"" + gdFrom +
                "\" onClick=\"gdMakeArticleActive( '" + jsVal + "' );\" " +
                " onContextMenu=\"gdMakeArticleActive( '" + jsVal + "' );\""
                + ">";

        closePrevSpan = true;

        head += string( "<div class=\"gddictname\"><span class=\"gdfromprefix\">" ) +
          Html::escape( tr( "From " ).toUtf8().data() ) + "</span>" +
          Html::escape( activeDict->getName().c_str() )
           + "</div>";

        head += "<span class=\"gdarticlebody gdlangfrom-";
        head += LangCoder::intToCode2( activeDict->getLangFrom() ).toLatin1().data();
        head += "\" lang=\"";
        head += LangCoder::intToCode2( activeDict->getLangTo() ).toLatin1().data();
        head += "\">";

        if ( errorString.size() )
        {
          head += "<div class=\"gderrordesc\">" +
            Html::escape( tr( "Query error: %1" ).arg( errorString ).toUtf8().data() )
          + "</div>";
        }

        Mutex::Lock _( dataMutex );

        size_t offset = data.size();

        data.resize( data.size() + head.size() + ( req.dataSize() > 0 ? req.dataSize() : 0 ) );

        memcpy( &data.front() + offset, head.data(), head.size() );

        if ( req.dataSize() > 0 )
          bodyRequests.front()->getDataSlice( 0, req.dataSize(),
                                              &data.front() + offset + head.size() );

        wasUpdated = true;

        foundAnyDefinitions = true;
      }
      //printf( "erasing..\n" );
      bodyRequests.pop_front();
      //printf( "erase done..\n" );
    }
    else
    {
        //printf( "one not finished.\n" );
        break;
    }
  }

  if ( bodyRequests.empty() )
  {
    // No requests left, end the article

    bodyDone = true;

    {
      string footer;

      if ( closePrevSpan )
      {
        footer += "</span></span>";
        closePrevSpan = false;
      }

      if ( !foundAnyDefinitions )
      {
        // No definitions were ever found, say so to the user.

        // Larger words are usually whole sentences - don't clutter the ouput
        // with their full bodies.
        footer += CGoldenDictMgr::makeNotFoundBody( word.size() < 40 ? word : "" );

        // When there were no definitions, we run stemmed search.
        stemmedWordFinder = new WordFinder( this );

        connect( stemmedWordFinder.get(), SIGNAL( finished() ),
                 this, SLOT( stemmedSearchFinished() ), Qt::QueuedConnection );

        stemmedWordFinder->stemmedMatch( word, activeDicts );
      }
      else
      {
        footer += "</body></html>";
      }

      Mutex::Lock _( dataMutex );

      size_t offset = data.size();

      data.resize( data.size() + footer.size() );

      memcpy( &data.front() + offset, footer.data(), footer.size() );
    }

    if ( stemmedWordFinder.get() )
      update();
    else
      finish();
  }
  else
  if ( wasUpdated )
    update();
}

void ArticleRequest::stemmedSearchFinished()
{
  // Got stemmed matching results

  WordFinder::SearchResults sr = stemmedWordFinder->getResults();

  string footer;

  bool continueMatching = false;

  if ( sr.size() )
  {
    footer += "<div class=\"gdstemmedsuggestion\"><span class=\"gdstemmedsuggestion_head\">" +
      Html::escape( tr( "Close words: " ).toUtf8().data() ) +
      "</span><span class=\"gdstemmedsuggestion_body\">";

    for( unsigned x = 0; x < sr.size(); ++x )
    {
      footer += linkWord( sr[ x ].first );

      if ( x != sr.size() - 1 )
      {
        footer += ", ";
      }
    }

    footer += "</span></div>";
  }

  splittedWords = splitIntoWords( word );

  if ( splittedWords.first.size() > 1 ) // Contains more than one word
  {
    disconnect( stemmedWordFinder.get(), SIGNAL( finished() ),
                this, SLOT( stemmedSearchFinished() ) );

    connect( stemmedWordFinder.get(), SIGNAL( finished() ),
             this, SLOT( individualWordFinished() ), Qt::QueuedConnection );

    currentSplittedWordStart = -1;
    currentSplittedWordEnd = currentSplittedWordStart;

    firstCompoundWasFound = false;

    compoundSearchNextStep( false );

    continueMatching = true;
  }

  if ( !continueMatching )
    footer += "</body></html>";

  {
    Mutex::Lock _( dataMutex );

    size_t offset = data.size();

    data.resize( data.size() + footer.size() );

    memcpy( &data.front() + offset, footer.data(), footer.size() );
  }

  if ( continueMatching )
    update();
  else
    finish();
}

void ArticleRequest::compoundSearchNextStep( bool lastSearchSucceeded )
{
  if ( !lastSearchSucceeded )
  {
    // Last search was unsuccessful. First, emit what we had.

    string footer;

    if ( lastGoodCompoundResult.size() ) // We have something to append
    {
//      printf( "Appending\n" );

      if ( !firstCompoundWasFound )
      {
        // Append the beginning
        footer += "<div class=\"gdstemmedsuggestion\"><span class=\"gdstemmedsuggestion_head\">" +
          Html::escape( tr( "Compound expressions: " ).toUtf8().data() ) +
          "</span><span class=\"gdstemmedsuggestion_body\">";

        firstCompoundWasFound = true;
      }
      else
      {
        // Append the separator
        footer += " / ";
      }

      footer += linkWord( lastGoodCompoundResult );

      lastGoodCompoundResult.clear();
    }

    // Then, start a new search for the next word, if possible

    if ( currentSplittedWordStart >= splittedWords.first.size() - 2 )
    {
      // The last word was the last possible to start from

      if ( firstCompoundWasFound )
        footer += "</span>";

      // Now add links to all the individual words. They conclude the result.

      footer += "<div class=\"gdstemmedsuggestion\"><span class=\"gdstemmedsuggestion_head\">" +
        Html::escape( tr( "Individual words: " ).toUtf8().data() ) +
        "</span><span class=\"gdstemmedsuggestion_body\">";

      footer += escapeSpacing( splittedWords.second[ 0 ] );

      for( int x = 0; x < splittedWords.first.size(); ++x )
      {
        footer += linkWord( splittedWords.first[ x ] );
        footer += escapeSpacing( splittedWords.second[ x + 1 ] );
      }

      footer += "</span>";

      footer += "</body></html>";

      appendToData( footer );

      finish();

      return;
    }

    if ( footer.size() )
    {
      appendToData( footer );
      update();
    }

    // Advance to the next word and start from looking up two words
    ++currentSplittedWordStart;
    currentSplittedWordEnd = currentSplittedWordStart + 1;
  }
  else
  {
    // Last lookup succeeded -- see if we can try the larger sequence

    if ( currentSplittedWordEnd < splittedWords.first.size() - 1 )
    {
      // We can, indeed.
      ++currentSplittedWordEnd;
    }
    else
    {
      // We can't. Emit what we have and start over.

      ++currentSplittedWordEnd; // So we could use the same code for result
                                // emitting

      // Initiate new lookup
      compoundSearchNextStep( false );

      return;
    }
  }

  // Build the compound sequence

  currentSplittedWordCompound = makeSplittedWordCompound();

  // Look it up

//  printf( "Looking up %s\n", qPrintable( currentSplittedWordCompound ) );

  stemmedWordFinder->prefixMatch( currentSplittedWordCompound, activeDicts, 40, // Would one be enough? Leave 40 to be safe.
                                  Dictionary::SuitableForCompoundSearching );
}

QString ArticleRequest::makeSplittedWordCompound()
{
  QString result;

  result.clear();

  for( int x = currentSplittedWordStart; x <= currentSplittedWordEnd; ++x )
  {
    result.append( splittedWords.first[ x ] );

    if ( x < currentSplittedWordEnd )
    {
      wstring ws( gd::toWString( splittedWords.second[ x + 1 ] ) );

      Folding::normalizeWhitespace( ws );

      result.append( gd::toQString( ws ) );
    }
  }

  return result;
}

void ArticleRequest::individualWordFinished()
{
  WordFinder::SearchResults const & results = stemmedWordFinder->getResults();

  if ( results.size() )
  {
    // Check if the aliases are acceptable
    wstring source = Folding::applySimpleCaseOnly( gd::toWString( currentSplittedWordCompound ) );

    bool hadSomething = false;

    for( unsigned x = 0; x < results.size(); ++x )
    {
      if ( results[ x ].second )
        continue; // We're not interested in suggestions

      wstring result( Folding::applySimpleCaseOnly( gd::toWString( results[ x ].first ) ) );

      if ( source.size() <= result.size() && result.compare( 0, source.size(), source ) == 0 )
      {
        // The resulting string begins with the source one

        hadSomething = true;

        if ( source.size() == result.size() )
        {
          // Got the match. No need to continue.
          lastGoodCompoundResult = currentSplittedWordCompound;
          break;
        }
      }
    }

    if ( hadSomething )
    {
      compoundSearchNextStep( true );
      return;
    }
  }

  compoundSearchNextStep( false );
}

void ArticleRequest::appendToData( std::string const & str )
{
  Mutex::Lock _( dataMutex );

  size_t offset = data.size();

  data.resize( data.size() + str.size() );

  memcpy( &data.front() + offset, str.data(), str.size() );

}

QPair< ArticleRequest::Words, ArticleRequest::Spacings > ArticleRequest::splitIntoWords( QString const & input )
{
  QPair< Words, Spacings > result;

  QChar const * ptr = input.data();

  for( ; ; )
  {
    QString spacing;

    for( ; ptr->unicode() && ( Folding::isPunct( ptr->unicode() ) || Folding::isWhitespace( ptr->unicode() ) ); ++ptr )
      spacing.append( *ptr );

    result.second.append( spacing );

    QString word;

    for( ; ptr->unicode() && !( Folding::isPunct( ptr->unicode() ) || Folding::isWhitespace( ptr->unicode() ) ); ++ptr )
      word.append( *ptr );

    if ( word.isEmpty() )
      break;

    result.first.append( word );
  }

  return result;
}

string ArticleRequest::linkWord( QString const & str )
{
  QUrl url;

  url.setScheme( "gdlookup" );
  url.setHost( "localhost" );
  QUrlQuery requ;
  requ.addQueryItem( "word", str );
  url.setQuery(requ);

  string escapedResult = Html::escape( str.toUtf8().data() );
  return string( "<a href=\"" ) + url.toEncoded().data() + "\">" + escapedResult +"</a>";
}

std::string ArticleRequest::escapeSpacing( QString const & str )
{
  QByteArray spacing = Html::escape( str.toUtf8().data() ).c_str();

  spacing.replace( "\n", "<br>" );

  return spacing.data();
}

QNetworkReply * ArticleNetworkAccessManager::createRequest( Operation op,
                                                            QNetworkRequest const & req,
                                                            QIODevice * outgoingData )
{
  if ( op == GetOperation )
  {
    if ( req.url().scheme() == "qrcx" )
    {
      // We have to override the local load policy for the qrc scheme, hence
      // we use qrcx and redirect it here back to qrc
      QUrl newUrl( req.url() );

      newUrl.setScheme( "qrc" );
      newUrl.setHost( "" );

      QNetworkRequest newReq( req );
      newReq.setUrl( newUrl );

      return QNetworkAccessManager::createRequest( op, newReq, outgoingData );
    }

    QString contentType;

    sptr< Dictionary::DataRequest > dr = getResource( req.url(), contentType );

    if ( dr.get() )
      return new ArticleResourceReply( this, req, dr, contentType );
  }

  if ( req.hasRawHeader( "Referer" ) )
  {
    QByteArray referer = req.rawHeader( "Referer" );

    QUrl refererUrl = QUrl::fromEncoded( referer );

    if ( !req.url().host().endsWith( refererUrl.host() ) &&
         getHostBase( req.url() ) != getHostBase( refererUrl ) )
    {
      //printf( "Blocking element %s\n", req.url().toEncoded().data() );

      return new BlockedNetworkReply( this );
    }
  }

  return QNetworkAccessManager::createRequest( op, req, outgoingData );
}

sptr< Dictionary::DataRequest > ArticleNetworkAccessManager::getResource(
  QUrl const & url, QString & contentType )
{
  //printf( "getResource: %ls\n", url.toString().toStdWString().c_str() );
  //printf( "scheme: %ls\n", url.scheme().toStdWString().c_str() );
  //printf( "host: %ls\n", url.host().toStdWString().c_str() );

  if ( url.scheme() == "gdlookup" )
  {
    contentType = "text/html";

    QMap< QString, QString > contexts;
    QString word, contextsEncoded;

    QUrlQuery qr(url);
    if ( qr.queryItemValue( "blank" ) == "1" )
        return dictMgr->makeEmptyPage();

    word = qr.queryItemValue( "word" );

    contextsEncoded = qr.queryItemValue( "contexts" );

    // Unpack contexts

    if ( contextsEncoded.size() )
    {
      QByteArray ba = QByteArray::fromBase64( contextsEncoded.toLatin1() );

      QBuffer buf( & ba );

      buf.open( QBuffer::ReadOnly );

      QDataStream stream( &buf );

      stream >> contexts;
    }

    if ( word.size() ) // Require group and word to be passed
      return dictMgr->makeDefinitionFor( word, contexts );
  }

  if ( ( url.scheme() == "bres" || url.scheme() == "gdau" ) &&
       url.path().size() )
  {
    //printf( "Get %s\n", req.url().host().toLocal8Bit().data() );
    //printf( "Get %s\n", req.url().path().toLocal8Bit().data() );

    string id = url.host().toStdString();

    bool search = ( id == "search" );

    if ( !search )
    {
      for( unsigned x = 0; x < dictMgr->dictionaries.size(); ++x )
        if ( dictMgr->dictionaries[ x ]->getId() == id )
          return  dictMgr->dictionaries[ x ]->getResource( url.path().mid( 1 ).toUtf8().data() );
    }
    else
    {
      // We don't do search requests for now
    }
  }

  return sptr< Dictionary::DataRequest >();
}

ArticleResourceReply::ArticleResourceReply( QObject * parent,
  QNetworkRequest const & netReq,
  sptr< Dictionary::DataRequest > const & req_,
  QString const & contentType ):
  QNetworkReply( parent ), req( req_ ), alreadyRead( 0 )
{
  setRequest( netReq );

  setOpenMode( ReadOnly );

  if ( contentType.size() )
    setHeader( QNetworkRequest::ContentTypeHeader, contentType );

  connect( req.get(), SIGNAL( updated() ),
           this, SLOT( reqUpdated() ) );

  connect( req.get(), SIGNAL( finished() ),
           this, SLOT( reqFinished() ) );

  if ( req->isFinished() || req->dataSize() > 0 )
  {
    connect( this, SIGNAL( readyReadSignal() ),
             this, SLOT( readyReadSlot() ), Qt::QueuedConnection );
    connect( this, SIGNAL( finishedSignal() ),
             this, SLOT( finishedSlot() ), Qt::QueuedConnection );

    emit readyReadSignal();

    if ( req->isFinished() )
    {
      emit finishedSignal();
      //printf( "In-place finish.\n" );
    }
  }
}

ArticleResourceReply::~ArticleResourceReply()
{
  req->cancel();
}

void ArticleResourceReply::reqUpdated()
{
  emit readyRead();
}

void ArticleResourceReply::reqFinished()
{
  emit readyRead();
  finishedSlot();
}

qint64 ArticleResourceReply::bytesAvailable() const
{
  long avail = req->dataSize();

  if ( avail < 0 )
    return 0;

  return (size_t) avail - alreadyRead +  QNetworkReply::bytesAvailable();
}

qint64 ArticleResourceReply::readData( char * out, qint64 maxSize )
{
  //printf( "====reading %d bytes\n", (int)maxSize );

  bool finished = req->isFinished();

  long avail = req->dataSize();

  if ( avail < 0 )
    return finished ? -1 : 0;

  size_t left = (size_t) avail - alreadyRead;

  size_t toRead = maxSize < left ? maxSize : left;

  req->getDataSlice( alreadyRead, toRead, out );

  alreadyRead += toRead;

  if ( !toRead && finished )
    return -1;
  else
    return toRead;
}

void ArticleResourceReply::readyReadSlot()
{
  readyRead();
}

void ArticleResourceReply::finishedSlot()
{
  if ( req->dataSize() < 0 )
    error( ContentNotFoundError );

  finished();
}

BlockedNetworkReply::BlockedNetworkReply( QObject * parent ): QNetworkReply( parent )
{
  setError( QNetworkReply::ContentOperationNotPermittedError, "Content Blocked" );

  connect( this, SIGNAL( finishedSignal() ), this, SLOT( finishedSlot() ),
           Qt::QueuedConnection );

  emit finishedSignal(); // This way we call readyRead()/finished() sometime later
}


void BlockedNetworkReply::finishedSlot()
{
  emit readyRead();
  emit finished();
}
