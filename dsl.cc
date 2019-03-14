/* This file is (c) 2008-2011 Konstantin Isakov <ikm@users.berlios.de>
 * Part of GoldenDict. Licensed under GPLv3 or later, see the LICENSE file */

#include "dsl.hh"
#include "dsl_details.hh"
#include "btreeidx.hh"
#include "folding.hh"
#include "utf8.hh"
#include "chunkedstorage.hh"
#include "dictzip.h"
#include "htmlescape.hh"
#include "iconv.hh"
#include "filetype.hh"
#include "fsencoding.hh"
#include "langcoder.hh"
#include "wstring_qt.hh"
#include "zipfile.hh"
#include "indexedzip.hh"

extern "C" {
#include <zlib.h>
}

#include <map>
#include <set>
#include <string>
#include <vector>
#include <list>
#include <cwctype>

#include <QSemaphore>
#include <QThreadPool>
#include <QAtomicInt>
#include <QUrl>

#include <QDir>
#include <QFileInfo>

namespace Dsl {

using namespace Details;

using std::map;
using std::pair;
using std::set;
using std::string;
using gd::wstring;
using gd::wchar;
using std::vector;
using std::list;

using BtreeIndexing::WordArticleLink;
using BtreeIndexing::IndexedWords;
using BtreeIndexing::IndexInfo;

namespace {

DEF_EX_STR( exCantReadFile, "Can't read file", Dictionary::Ex )

enum
{
    Signature = 0x584c5344, // DSLX on little-endian, XLSD on big-endian
            CurrentFormatVersion = 14 + BtreeIndexing::FormatVersion + Folding::Version,
            CurrentZipSupportVersion = 1
};

struct IdxHeader
{
    uint32_t signature; // First comes the signature, DSLX
    uint32_t formatVersion; // File format version (CurrentFormatVersion)
    uint32_t zipSupportVersion; // Zip support version -- narrows down reindexing
    // when it changes only for dictionaries with the
    // zip files
    int dslEncoding; // Which encoding is used for the file indexed
    uint32_t chunksOffset; // The offset to chunks' storage
    uint32_t hasAbrv; // Non-zero means file has abrvs at abrvAddress
    uint32_t abrvAddress; // Address of abrv map in the chunked storage
    uint32_t indexBtreeMaxElements; // Two fields from IndexInfo
    uint32_t indexRootOffset;
    uint32_t articleCount; // Number of articles this dictionary has
    uint32_t wordCount; // Number of headwords this dictionary has
    uint32_t langFrom;  // Source language
    uint32_t langTo;    // Target language
    uint32_t hasZipFile; // Non-zero means there's a zip file with resources
    // present
    uint32_t zipIndexBtreeMaxElements; // Two fields from IndexInfo of the zip
    // resource index.
    uint32_t zipIndexRootOffset;
}
__attribute__((packed))
;

bool indexIsOldOrBad( string const & indexFile, bool hasZipFile )
{
    File::Class idx( indexFile, "rb" );

    IdxHeader header {};

    return (idx.readRecords( &header, sizeof( header ), 1 ) != 1) ||
            (header.signature != Signature) ||
            (header.formatVersion != CurrentFormatVersion) ||
            (static_cast<bool>(header.hasZipFile) != hasZipFile) ||
            ( hasZipFile && header.zipSupportVersion != CurrentZipSupportVersion );
}

class DslDictionary: public BtreeIndexing::BtreeDictionary
{
    Mutex idxMutex;
    File::Class idx;
    IdxHeader idxHeader;
    sptr< ChunkedStorage::Reader > chunks;
    string dictionaryName;
    map< string, string > abrv;
    Mutex dzMutex;
    dictData * dz;
    Mutex resourceZipMutex;
    IndexedZip resourceZip;
    BtreeIndex resourceZipIndex;
    bool dictionaryIconLoaded;

    QAtomicInt deferredInitDone;
    Mutex deferredInitMutex;
    bool deferredInitRunnableStarted;
    QSemaphore deferredInitRunnableExited;

    string initError;

public:

    DslDictionary( string const & id, string const & indexFile,
                   vector< string > const & dictionaryFiles );

    void deferredInit() override;

    ~DslDictionary() override;

    string getName() override
    { return dictionaryName; }

    map< Dictionary::Property, string > getProperties() override
    { return map< Dictionary::Property, string >(); }

    unsigned long getArticleCount() override
    { return idxHeader.articleCount; }

    unsigned long getWordCount() override
    { return idxHeader.wordCount; }

    inline quint32 getLangFrom() const override
    { return idxHeader.langFrom; }

    inline quint32 getLangTo() const override
    { return idxHeader.langTo; }

    sptr< Dictionary::DataRequest > getArticle( wstring const &,
                                                vector< wstring > const & alts,
                                                wstring const & ) override;

    sptr< Dictionary::DataRequest > getResource( string const & name ) override;

    DslDictionary(const DslDictionary &) = delete;
    DslDictionary& operator =(DslDictionary const&) = delete;
    DslDictionary(DslDictionary&&) = delete;
    DslDictionary& operator=(DslDictionary&&) = delete;

private:

    string const & ensureInitDone() override;
    void doDeferredInit();

    /// Loads the article. Does not process the DSL language.
    void loadArticle( uint32_t address,
                      wstring const & requestedHeadwordFolded,
                      wstring & tildeValue,
                      wstring & displayedHeadword,
                      unsigned & headwordIndex,
                      wstring & articleText );

    /// Converts DSL language to an Html.
    string dslToHtml( wstring const & );

    // Parts of dslToHtml()
    string nodeToHtml( ArticleDom::Node const & );
    string processNodeChildren( ArticleDom::Node const & node );

    friend class DslArticleRequest;
    friend class DslResourceRequest;
    friend class DslDeferredInitRunnable;
};

DslDictionary::DslDictionary( string const & id,
                              string const & indexFile,
                              vector< string > const & dictionaryFiles ):
    BtreeDictionary( id, dictionaryFiles ),
    idx( indexFile, "rb" ),
    idxHeader( idx.read< IdxHeader >() ),
    dz( nullptr ),
    dictionaryIconLoaded( false ),
    deferredInitRunnableStarted( false )
{
    // Read the dictionary name

    idx.seek( sizeof( idxHeader ) );

    vector< char > dName( idx.read< uint32_t >() );
    idx.read( &dName.front(), dName.size() );
    dictionaryName = string( &dName.front(), dName.size() );

    // Everything else would be done in deferred init
}

DslDictionary::~DslDictionary()
{
    Mutex::Lock _( deferredInitMutex );

    // Wait for init runnable to complete if it was ever started
    if ( deferredInitRunnableStarted )
        deferredInitRunnableExited.acquire();

    if ( dz )
        dict_data_close( dz );
}

//////// DslDictionary::deferredInit()

class DslDeferredInitRunnable: public QRunnable
{
    DslDictionary & dictionary;
    QSemaphore & hasExited;

public:

    DslDeferredInitRunnable( DslDictionary & dictionary_,
                             QSemaphore & hasExited_ ):
        dictionary( dictionary_ ), hasExited( hasExited_ )
    {}

    ~DslDeferredInitRunnable() override
    {
        hasExited.release();
    }

    void run() override
    {
        dictionary.doDeferredInit();
    }

    DslDeferredInitRunnable(const DslDeferredInitRunnable &) = delete;
    DslDeferredInitRunnable& operator =(DslDeferredInitRunnable const&) = delete;
    DslDeferredInitRunnable(DslDeferredInitRunnable&&) = delete;
    DslDeferredInitRunnable& operator=(DslDeferredInitRunnable&&) = delete;

};

void DslDictionary::deferredInit()
{
    if ( deferredInitDone.load() == 0 )
    {
        Mutex::Lock _( deferredInitMutex );

        if ( deferredInitDone.load() != 0 )
            return;

        if ( !deferredInitRunnableStarted )
        {
            QThreadPool::globalInstance()->start(
                        new DslDeferredInitRunnable( *this, deferredInitRunnableExited ),
                        -1000 );
            deferredInitRunnableStarted = true;
        }
    }
}


string const & DslDictionary::ensureInitDone()
{
    // Simple, really.
    doDeferredInit();

    return initError;
}

void DslDictionary::doDeferredInit()
{
    if ( deferredInitDone.load() == 0 )
    {
        Mutex::Lock _( deferredInitMutex );

        if ( deferredInitDone.load() != 0 )
            return;

        // Do deferred init

        try
        {
            // Don't lock index file - no one should be working with it until
            // the init is complete.
            //Mutex::Lock _( idxMutex );

            chunks = new ChunkedStorage::Reader( idx, idxHeader.chunksOffset );

            // Open the .dict file

            dz = dict_data_open( getDictionaryFilenames()[ 0 ].c_str(), 0 );

            if ( !dz )
                throw exCantReadFile( getDictionaryFilenames()[ 0 ] );

            // Read the abrv, if any

            if ( idxHeader.hasAbrv )
            {
                vector< char > chunk;

                char * abrvBlock = chunks->getBlock( idxHeader.abrvAddress, chunk );

                uint32_t total;
                memcpy( &total, abrvBlock, sizeof( uint32_t ) );
                abrvBlock += sizeof( uint32_t );

                //printf( "Loading %u abbrv\n", total );

                while( total-- )
                {
                    uint32_t keySz;
                    memcpy( &keySz, abrvBlock, sizeof( uint32_t ) );
                    abrvBlock += sizeof( uint32_t );

                    char * key = abrvBlock;

                    abrvBlock += keySz;

                    uint32_t valueSz;
                    memcpy( &valueSz, abrvBlock, sizeof( uint32_t ) );
                    abrvBlock += sizeof( uint32_t );

                    abrv[ string( key, keySz ) ] = string( abrvBlock, valueSz );

                    abrvBlock += valueSz;
                }
            }

            // Initialize the index

            openIndex( IndexInfo( idxHeader.indexBtreeMaxElements,
                                  idxHeader.indexRootOffset ),
                       idx, idxMutex );

            // Open a resource zip file, if there's one

            if ( idxHeader.hasZipFile &&
                 ( idxHeader.zipIndexBtreeMaxElements ||
                   idxHeader.zipIndexRootOffset ) )
            {
                resourceZip.openIndex( IndexInfo( idxHeader.zipIndexBtreeMaxElements,
                                                  idxHeader.zipIndexRootOffset ),
                                       idx, idxMutex );

                QString zipName = QDir::fromNativeSeparators(
                                      QFile::decodeName( getDictionaryFilenames().back().c_str() ) );

                if ( zipName.endsWith( ".zip", Qt::CaseInsensitive ) ) // Sanity check
                    resourceZip.openZipFile( zipName );
            }
        }
        catch( std::exception & e )
        {
            initError = e.what();
        }
        catch( ... )
        {
            initError = "Unknown error";
        }

        deferredInitDone.ref();
    }
}

/// Determines whether or not this char is treated as whitespace for dsl
/// parsing or not. We can't rely on any Unicode standards here, since the
/// only standard that matters here is the original Dsl compiler's insides.
/// Some dictionaries, for instance, are known to specifically use a non-
/// breakable space (0xa0) to indicate that a headword begins with a space,
/// so nbsp is not a whitespace character for Dsl compiler.
/// For now we have only space and tab, since those are most likely the only
/// ones recognized as spaces by that compiler.
bool isDslWs( wchar ch )
{
    switch( ch )
    {
        case ' ':
        case '\t':
            return true;
        default:
            return false;
    }
}

void DslDictionary::loadArticle( uint32_t address,
                                 wstring const & requestedHeadwordFolded,
                                 wstring & tildeValue,
                                 wstring & displayedHeadword,
                                 unsigned & headwordIndex,
                                 wstring & articleText )
{
    wstring articleData;

    {
        vector< char > chunk;

        char * articleProps;

        {
            Mutex::Lock _( idxMutex );

            articleProps = chunks->getBlock( address, chunk );
        }

        uint32_t articleOffset, articleSize;

        memcpy( &articleOffset, articleProps, sizeof( articleOffset ) );
        memcpy( &articleSize, articleProps + sizeof( articleOffset ),
                sizeof( articleSize ) );

        //printf( "offset = %x\n", articleOffset );


        char * articleBody;

        {
            Mutex::Lock _( dzMutex );

            articleBody = dict_data_read_( dz, articleOffset, articleSize, nullptr, nullptr );
        }

        if ( !articleBody )
            throw exCantReadFile( getDictionaryFilenames()[ 0 ] );

        try
        {
            articleData =
                    DslIconv::toWstring(
                        DslIconv::getEncodingNameFor( DslEncoding( idxHeader.dslEncoding ) ),
                        articleBody, articleSize );
            free( articleBody );
        }
        catch( ... )
        {
            free( articleBody );
            throw;
        }
    }

    size_t pos = 0;
    bool hadFirstHeadword = false;
    bool foundDisplayedHeadword = false;

    wstring tildeValueWithUnsorted; // This one has unsorted parts left
    for( headwordIndex = 0; ; )
    {
        size_t begin = pos;

        pos = articleData.find_first_of( GD_NATIVE_TO_WS( L"\n\r" ), begin );

        if ( pos == wstring::npos )
            pos = articleData.size();

        if ( !foundDisplayedHeadword )
        {
            // Process the headword

            wstring rawHeadword = wstring( articleData, begin, pos - begin );

            if ( !hadFirstHeadword )
            {
                // We need our tilde expansion value
                tildeValue = rawHeadword;

                list< wstring > lst;

                expandOptionalParts( tildeValue, lst );

                if ( !lst.empty() ) // Should always be
                    tildeValue = lst.front();

                tildeValueWithUnsorted = tildeValue;

                processUnsortedParts( tildeValue, false );
            }

            wstring str = rawHeadword;

            if ( hadFirstHeadword )
                expandTildes( str, tildeValueWithUnsorted );

            processUnsortedParts( str, true );

            str = Folding::applySimpleCaseOnly( str );

            list< wstring > lst;
            expandOptionalParts( str, lst );

            // Does one of the results match the requested word? If so, we'd choose
            // it as our headword.

            for( auto & i : lst )
            {
                unescapeDsl( i );
                normalizeHeadword( i );

                if ( Folding::trimWhitespace( i ) == requestedHeadwordFolded )
                {
                    // Found it. Now we should make a displayed headword for it.
                    if ( hadFirstHeadword )
                        expandTildes( rawHeadword, tildeValueWithUnsorted );

                    processUnsortedParts( rawHeadword, false );

                    displayedHeadword = rawHeadword;

                    foundDisplayedHeadword = true;
                    break;
                }
            }

            if ( !foundDisplayedHeadword )
            {
                ++headwordIndex;
                hadFirstHeadword = true;
            }
        }

        if ( pos == articleData.size() )
            break;

        // Skip \n\r

        if ( articleData[ pos ] == '\r' )
            ++pos;

        if ( pos != articleData.size() )
        {
            if ( articleData[ pos ] == '\n' )
                ++pos;
        }

        if ( pos == articleData.size() || isDslWs( articleData[ pos ] ) )
        {
            // Ok, it's either end of article, or the begining of the article's text
            break;
        }
    }

    if ( !foundDisplayedHeadword )
    {
        // This is strange. Anyway, use tilde expansion value, it's better
        // than nothing.
        displayedHeadword = tildeValue;
    }

    if ( pos != articleData.size() )
        articleText = wstring( articleData, pos );
    else
        articleText.clear();
}

string DslDictionary::dslToHtml( wstring const & str )
{
    // Normalize the string
    wstring normalizedStr = gd::toWString( gd::toQString( str ).normalized( QString::NormalizationForm_C ) );

    ArticleDom dom( normalizedStr );

    string html = processNodeChildren( dom.root );

    // Lines seem to indicate paragraphs in Dsls, so we enclose each line within
    // a <p></p>.

    for( size_t x = html.size(); x--; )
        if ( html[ x ] == '\n' )
            html.insert( x + 1, "</p><p>" );

    return
        #if 0 // Enable this to enable dsl source in html as a comment
            "<!-- DSL Source:\n" + Utf8::encode( str ) + "\n-->"
        #endif
                                                         "<p>" + html + "</p>";
}

string DslDictionary::processNodeChildren( ArticleDom::Node const & node )
{
    string result;

    for( const auto & i : node )
        result += nodeToHtml( i );

    return result;
}

string DslDictionary::nodeToHtml( ArticleDom::Node const & node )
{
    if ( !node.isTag )
        return Html::escape( Utf8::encode( node.text ) );

    string result;

    if ( node.tagName == GD_NATIVE_TO_WS( L"b" ) ) {
        result += R"(<b class="dsl_b">)";
        result += processNodeChildren( node );
        result += "</b>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"i" ) ) {
        result += R"(<i class="dsl_i">)";
        result += processNodeChildren( node );
        result += "</i>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"u" ) )
    {
        string nodeText = processNodeChildren( node );

        if ( !nodeText.empty() && isDslWs( nodeText[ 0 ] ) )
            result.push_back( ' ' ); // Fix a common problem where in "foo[i] bar[/i]"
        // the space before "bar" gets underlined.

        result += R"(<span class="dsl_u">)";
        result += nodeText;
        result += "</span>";
    }
    else if ( node.tagName == GD_NATIVE_TO_WS( L"c" ) )
    {
        result += R"(<font color=")";
        if( !node.tagAttrs.empty() )
            result += Html::escape( Utf8::encode( node.tagAttrs ) );
        else
            result += "c_default_color";
        result += R"(">)";
        result += processNodeChildren( node );
        result += "</font>";
    }
    else if ( node.tagName == GD_NATIVE_TO_WS( L"*" ) ) {
        result += R"(<span class="dsl_opt">)";
        result += processNodeChildren( node );
        result += "</span>";
    } else if ( node.tagName.size() == 2 && node.tagName[ 0 ] == L'm' &&
                iswdigit( node.tagName[ 1 ] ) ) {
        result += R"(<div class="dsl_)";
        result += Utf8::encode( node.tagName );
        result += R"(">)";
        result += processNodeChildren( node );
        result += "</div>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"trn" ) ) {
        result += R"(<span class="dsl_trn">)";
        result += processNodeChildren( node );
        result += "</span>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"ex" ) ) {
        result += R"(<span class="dsl_ex">)";
        result += processNodeChildren( node );
        result += "</span>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"com" ) ) {
        result += R"(<span class="dsl_com">)";
        result += processNodeChildren( node );
        result += "</span>";
    } else if ( node.tagName == GD_NATIVE_TO_WS( L"s" ) ) {
        string filename = Utf8::encode( node.renderAsText() );

        if ( Filetype::isNameOfPicture( filename ) )
        {
            QUrl url;
            url.setScheme( "bres" );
            url.setHost( QString::fromUtf8( getId().c_str() ) );
            url.setPath( QString::fromUtf8( filename.c_str() ) );

            result += R"(<img src=")";
            result += url.toEncoded().constData();
            result += R"(" alt=")";
            result += Html::escape( filename );
            result += R"("/>)";
        }
        else
        {
            // Unknown file type, downgrade to a hyperlink

            QUrl url;
            url.setScheme( "bres" );
            url.setHost( QString::fromUtf8( getId().c_str() ) );
            url.setPath( QString::fromUtf8( filename.c_str() ) );

            result += R"(<a class="dsl_s" href=")";
            result += url.toEncoded().constData();
            result += R"(">)";
            result += processNodeChildren( node );
            result += "</a>";
        }
    }
    else
        if ( node.tagName == GD_NATIVE_TO_WS( L"url" ) ) {
            result += R"(<a class="dsl_url" href=")";
            result += Html::escape( Utf8::encode( node.renderAsText() ) );
            result += R"(">)";
            result += processNodeChildren( node );
            result += "</a>";
        } else if ( node.tagName == GD_NATIVE_TO_WS( L"!trs" ) ) {
            result += R"(<span class="dsl_trs">)";
            result += processNodeChildren( node );
            result += "</span>";
        } else if ( node.tagName == GD_NATIVE_TO_WS( L"p") )
        {
            result += R"(<span class="dsl_p")";

            string val = Utf8::encode( node.renderAsText() );

            // If we have such a key, display a title

            map< string, string >::const_iterator i = abrv.find( val );

            if ( i != abrv.end() )
            {
                string title;

                if ( Utf8::decode( i->second ).size() < 70 )
                {
                    // Replace all spaces with non-breakable ones, since that's how
                    // Lingvo shows tooltips
                    title.reserve( i->second.size() );

                    for( char const * c = i->second.c_str(); *c; ++c )
                        if ( *c == ' ' || *c == '\t' )
                        {
                            // u00A0 in utf8
                            title.push_back( 0xC2 );
                            title.push_back( 0xA0 );
                        }
                        else
                            title.push_back( *c );
                }
                else
                    title = i->second;

                result += R"( title=")";
                result += Html::escape( title );
                result += R"(")";
            }

            result += ">" + processNodeChildren( node ) + "</span>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"'" ) )
        {
            result += R"(<span class="dsl_stress">)";
            result += processNodeChildren( node );
            result += Utf8::encode( wstring( 1, 0x301 ) );
            result += "</span>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"lang" ) )
        {
            result += R"(<span class="dsl_lang">)";
            result += processNodeChildren( node );
            result += "</span>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"ref" ) )
        {
            QUrl url;

            url.setScheme( "gdlookup" );
            url.setHost( "localhost" );
            QUrlQuery urlq;
            urlq.addQueryItem("word", gd::toQString( node.renderAsText() ));
            url.setQuery(urlq);

            result += R"(<a class="dsl_ref" href=")";
            result += url.toEncoded().constData();
            result += R"(")";
            result += processNodeChildren( node );
            result += "</a>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"sub" ) )
        {
            result += "<sub>";
            result += processNodeChildren( node );
            result += "</sub>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"sup" ) )
        {
            result += "<sup>";
            result += processNodeChildren( node );
            result += "</sup>";
        }
        else if ( node.tagName == GD_NATIVE_TO_WS( L"t" ) )
        {
            result += R"(<span class="dsl_t">)";
            result += processNodeChildren( node );
            result += "</span>";
        }
        else {
            result += R"(<span class="dsl_unknown">)";
            result += processNodeChildren( node );
            result += "</span>";
        }

    return result;
}

/// DslDictionary::getArticle()

class DslArticleRequest;

class DslArticleRequestRunnable: public QRunnable
{
    DslArticleRequest & r;
    QSemaphore & hasExited;

public:

    DslArticleRequestRunnable( DslArticleRequest & r_,
                               QSemaphore & hasExited_ ): r( r_ ),
        hasExited( hasExited_ )
    {}

    ~DslArticleRequestRunnable() override
    {
        hasExited.release();
    }

    void run() override;

    DslArticleRequestRunnable(const DslArticleRequestRunnable &) = delete;
    DslArticleRequestRunnable& operator =(DslArticleRequestRunnable const&) = delete;
    DslArticleRequestRunnable(DslArticleRequestRunnable&&) = delete;
    DslArticleRequestRunnable& operator=(DslArticleRequestRunnable&&) = delete;

};

class DslArticleRequest: public Dictionary::DataRequest
{
    friend class DslArticleRequestRunnable;

    wstring word;
    vector< wstring > alts;
    DslDictionary & dict;

    QAtomicInt isCancelled;
    QSemaphore hasExited;

public:

    DslArticleRequest( wstring const & word_,
                       vector< wstring > const & alts_,
                       DslDictionary & dict_ ):
        word( word_ ), alts( alts_ ), dict( dict_ )
    {
        QThreadPool::globalInstance()->start(
                    new DslArticleRequestRunnable( *this, hasExited ) );
    }

    void run(); // Run from another thread by DslArticleRequestRunnable

    void cancel() override
    {
        isCancelled.ref();
    }

    ~DslArticleRequest() override
    {
        isCancelled.ref();
        hasExited.acquire();
    }

    DslArticleRequest(const DslArticleRequest &) = delete;
    DslArticleRequest& operator =(DslArticleRequest const&) = delete;
    DslArticleRequest(DslArticleRequest&&) = delete;
    DslArticleRequest& operator=(DslArticleRequest&&) = delete;

};

void DslArticleRequestRunnable::run()
{
    r.run();
}

void DslArticleRequest::run()
{
    if ( isCancelled.load() != 0 )
    {
        finish();
        return;
    }

    if ( !dict.ensureInitDone().empty() )
    {
        setErrorString( QString::fromUtf8( dict.ensureInitDone().c_str() ) );
        finish();
        return;
    }

    vector< WordArticleLink > chain = dict.findArticles( word );

    for( const auto & alt : alts )
    {
        /// Make an additional query for each alt

        vector< WordArticleLink > altChain = dict.findArticles( alt );

        chain.insert( chain.end(), altChain.cbegin(), altChain.cend() );
    }

    // Some synonyms make it that the articles appear several times. We combat
    // this by only allowing them to appear once. Dsl treats different headwords
    // of the same article as different articles, so we also include headword
    // index here.
    set< pair< uint32_t, unsigned > > articlesIncluded;

    wstring wordCaseFolded = Folding::applySimpleCaseOnly( word );

    for( const auto & cx : chain )
    {
        // Check if we're cancelled occasionally
        if ( isCancelled.load() != 0 )
        {
            finish();
            return;
        }

        // Grab that article

        wstring tildeValue;
        wstring displayedHeadword;
        wstring articleBody;
        unsigned headwordIndex;

        dict.loadArticle( cx.articleOffset, wordCaseFolded, tildeValue,
                          displayedHeadword, headwordIndex, articleBody );

        if ( !articlesIncluded.insert( std::make_pair( cx.articleOffset,
                                                       headwordIndex ) ).second )
            continue; // We already have this article in the body.

        string articleText;

        articleText += R"(<span class="dsl_article">)";
        articleText += R"(<div class="dsl_headwords">)";

        articleText += dict.dslToHtml( displayedHeadword );

        articleText += "</div>";

        expandTildes( articleBody, tildeValue );

        articleText += R"(<div class="dsl_definition">)";
        articleText += dict.dslToHtml( articleBody );
        articleText += "</div>";
        articleText += "</span>";

        Mutex::Lock _( dataMutex );

        data.resize( data.size() + articleText.size() );

        memcpy( &data.front() + data.size() - articleText.size(),
                articleText.data(), articleText.size() );

        hasAnyData = true;
    }

    finish();
}

sptr< Dictionary::DataRequest > DslDictionary::getArticle( wstring const & word,
                                                           vector< wstring > const & alts,
                                                           wstring const & )
{
    return new DslArticleRequest( word, alts, *this );
}

void loadFromFile( string const & n, vector< char > & data )
{
    File::Class f( n, "rb" );

    f.seekEnd();

    data.resize( f.tell() );

    f.rewind();

    f.read( &data.front(), data.size() );
}

//// DslDictionary::getResource()

class DslResourceRequest;

class DslResourceRequestRunnable: public QRunnable
{
    DslResourceRequest & r;
    QSemaphore & hasExited;

public:

    DslResourceRequestRunnable( DslResourceRequest & r_,
                                QSemaphore & hasExited_ ): r( r_ ),
        hasExited( hasExited_ )
    {}

    ~DslResourceRequestRunnable() override
    {
        hasExited.release();
    }

    void run() override;

    DslResourceRequestRunnable(const DslResourceRequestRunnable &) = delete;
    DslResourceRequestRunnable& operator =(DslResourceRequestRunnable const&) = delete;
    DslResourceRequestRunnable(DslResourceRequestRunnable&&) = delete;
    DslResourceRequestRunnable& operator=(DslResourceRequestRunnable&&) = delete;

};

class DslResourceRequest: public Dictionary::DataRequest
{
    friend class DslResourceRequestRunnable;

    DslDictionary & dict;

    string resourceName;

    QAtomicInt isCancelled;
    QSemaphore hasExited;

public:

    DslResourceRequest( DslDictionary & dict_,
                        string const & resourceName_ ):
        dict( dict_ ),
        resourceName( resourceName_ )
    {
        QThreadPool::globalInstance()->start(
                    new DslResourceRequestRunnable( *this, hasExited ) );
    }

    void run(); // Run from another thread by DslResourceRequestRunnable

    void cancel() override
    {
        isCancelled.ref();
    }

    ~DslResourceRequest() override
    {
        isCancelled.ref();
        hasExited.acquire();
    }

    DslResourceRequest(const DslResourceRequest &) = delete;
    DslResourceRequest& operator =(DslResourceRequest const&) = delete;
    DslResourceRequest(DslResourceRequest&&) = delete;
    DslResourceRequest& operator=(DslResourceRequest&&) = delete;

};

void DslResourceRequestRunnable::run()
{
    r.run();
}

void DslResourceRequest::run()
{
    // Some runnables linger enough that they are cancelled before they start
    if ( isCancelled.load() != 0 )
    {
        finish();
        return;
    }

    if ( !dict.ensureInitDone().empty() )
    {
        setErrorString( QString::fromUtf8( dict.ensureInitDone().c_str() ) );
        finish();
        return;
    }

    string n =
            FsEncoding::dirname( dict.getDictionaryFilenames()[ 0 ] ) +
            FsEncoding::separator() +
            FsEncoding::encode( resourceName );

    //printf( "n is %s\n", n.c_str() );

    try
    {
        try
        {
            Mutex::Lock _( dataMutex );

            loadFromFile( n, data );
        }
        catch( File::exCantOpen & )
        {
            n = dict.getDictionaryFilenames()[ 0 ] + ".files" +
                FsEncoding::separator() +
                FsEncoding::encode( resourceName );

            try
            {
                Mutex::Lock _( dataMutex );

                loadFromFile( n, data );
            }
            catch( File::exCantOpen & )
            {
                // Try reading from zip file

                if ( dict.resourceZip.isOpen() )
                {
                    Mutex::Lock _( dict.resourceZipMutex );

                    Mutex::Lock __( dataMutex );

                    if ( !dict.resourceZip.loadFile( Utf8::decode( resourceName ), data ) )
                        throw; // Make it fail since we couldn't read the archive
                }
                else
                    throw;
            }
        }

        Mutex::Lock _( dataMutex );

        hasAnyData = true;
    }
    catch( File::Ex & )
    {
        // No such resource -- we don't set the hasAnyData flag then
    }
    catch( Utf8::exCantDecode & )
    {
        // Failed to decode some utf8 -- probably the resource name is no good
    }

    finish();
}

sptr< Dictionary::DataRequest > DslDictionary::getResource( string const & name )
{
    return new DslResourceRequest( *this, name );
}

} // anonymous namespace

static bool tryPossibleName( string const & name, string & copyTo )
{
    if ( File::exists( name ) )
    {
        copyTo = name;

        return true;
    }

    return false;
}

vector< sptr< Dictionary::Class > > makeDictionaries(
        vector< string > const & fileNames,
        string const & indicesDir,
        Dictionary::Initializing & initializing )
{
    vector< sptr< Dictionary::Class > > dictionaries;

    for( const auto & fName : fileNames )
    {
        // Try .dsl and .dsl.dz suffixes

        bool uncompressedDsl = ( fName.size() >= 4 &&
                                 strcasecmp( fName.c_str() + ( fName.size() - 4 ), ".dsl" ) == 0 );
        if ( !uncompressedDsl &&
             ( fName.size() < 7 ||
               strcasecmp( fName.c_str() + ( fName.size() - 7 ), ".dsl.dz" ) != 0 ) )
            continue;

        // Make sure it's not an abbreviation file

        int extSize = ( uncompressedDsl ? 4 : 7 );
        if ( fName.size() - extSize >= 5 &&
             strncasecmp( fName.c_str() + fName.size() - extSize - 5, "_abrv", 5 ) == 0 )
        {
            // It is, skip it
            continue;
        }

        unsigned atLine = 0; // Indicates current line in .dsl, for debug purposes

        try
        {
            vector< string > dictFiles( 1, fName );

            // Check if there is an 'abrv' file present
            string baseName = ( fName[ fName.size() - 4 ] == '.' ) ?
                        string( fName, 0, fName.size() - 4 ) : string( fName, 0, fName.size() - 7 );

            string abrvFileName;

            if ( tryPossibleName( baseName + "_abrv.dsl", abrvFileName ) ||
                 tryPossibleName( baseName + "_abrv.dsl.dz", abrvFileName ) ||
                 tryPossibleName( baseName + "_ABRV.DSL", abrvFileName ) ||
                 tryPossibleName( baseName + "_ABRV.DSL.DZ", abrvFileName ) ||
                 tryPossibleName( baseName + "_ABRV.DSL.dz", abrvFileName ) )
                dictFiles.push_back( abrvFileName );

            string dictId = Dictionary::makeDictionaryId( dictFiles );

            // See if there's a zip file with resources present. If so, include it.

            string zipFileName;

            if ( tryPossibleName( baseName + ".dsl.files.zip", zipFileName ) ||
                 tryPossibleName( baseName + ".dsl.dz.files.zip", zipFileName ) ||
                 tryPossibleName( baseName + ".DSL.FILES.ZIP", zipFileName ) ||
                 tryPossibleName( baseName + ".DSL.DZ.FILES.ZIP", zipFileName ) )
                dictFiles.push_back( zipFileName );

            string indexFile = indicesDir + dictId;

            if ( Dictionary::needToRebuildIndex( dictFiles, indexFile ) ||
                 indexIsOldOrBad( indexFile, !zipFileName.empty() ) )
            {
                DslScanner scanner( fName );

                try { // Here we intercept any errors during the read to save line at
                    // which the incident happened. We need alive scanner for that.

                    if ( scanner.getDictionaryName() == GD_NATIVE_TO_WS( L"Abbrev" ) )
                        continue; // For now just skip abbreviations

                    // Building the index
                    initializing.indexingDictionary( Utf8::encode( scanner.getDictionaryName() ) );

                    //printf( "Dictionary name: %ls\n", scanner.getDictionaryName().c_str() );

                    File::Class idx( indexFile, "wb" );

                    IdxHeader idxHeader {};

                    memset( &idxHeader, 0, sizeof( idxHeader ) );

                    // We write a dummy header first. At the end of the process the header
                    // will be rewritten with the right values.

                    idx.write( idxHeader );

                    string dictionaryName = Utf8::encode( scanner.getDictionaryName() );

                    idx.write( static_cast<uint32_t>(dictionaryName.size()) );
                    idx.write( dictionaryName.data(), dictionaryName.size() );

                    idxHeader.dslEncoding = scanner.getEncoding();

                    IndexedWords indexedWords;

                    ChunkedStorage::Writer chunks( idx );

                    // Read the abbreviations

                    if ( !abrvFileName.empty() )
                    {
                        try
                        {
                            DslScanner abrvScanner( abrvFileName );

                            map< string, string > abrv;

                            wstring curString;
                            size_t curOffset;

                            for( ; ; )
                            {
                                // Skip any whitespace
                                if ( !abrvScanner.readNextLine( curString, curOffset ) )
                                    break;
                                if ( curString.empty() || isDslWs( curString[ 0 ] ) )
                                    continue;

                                list< wstring > keys;

                                bool eof = false;

                                // Insert the key and read more, or get to the definition
                                for( ; ; )
                                {
                                    processUnsortedParts( curString, true );

                                    if ( !keys.empty() )
                                        expandTildes( curString, keys.front() );

                                    expandOptionalParts( curString, keys );

                                    if ( !abrvScanner.readNextLine( curString, curOffset ) || curString.empty() )
                                    {
                                        qWarning() << "Premature end of file "
                                                   << abrvFileName.c_str();
                                        eof = true;
                                        break;
                                    }

                                    if ( isDslWs( curString[ 0 ] ) )
                                        break;
                                }

                                if ( eof )
                                    break;

                                curString.erase( 0, curString.find_first_not_of( GD_NATIVE_TO_WS( L" \t" ) ) );

                                if ( !keys.empty() )
                                    expandTildes( curString, keys.front() );

                                // If the string has any dsl markup, we strip it
                                string value = Utf8::encode( ArticleDom( curString ).root.renderAsText() );

                                for( auto & key : keys )
                                {
                                    unescapeDsl( key );
                                    normalizeHeadword( key );

                                    abrv[ Utf8::encode( Folding::trimWhitespace( key ) ) ] = value;
                                }
                            }

                            idxHeader.hasAbrv = 1;
                            idxHeader.abrvAddress = chunks.startNewBlock();

                            uint32_t sz = abrv.size();

                            chunks.addToBlock( &sz, sizeof( uint32_t ) );

                            for( map< string, string >::const_iterator i = abrv.begin();
                                 i != abrv.end(); ++i )
                            {
                                //              printf( "%s:%s\n", i->first.c_str(), i->second.c_str() );

                                sz = i->first.size();
                                chunks.addToBlock( &sz, sizeof( uint32_t ) );
                                chunks.addToBlock( i->first.data(), sz );
                                sz = i->second.size();
                                chunks.addToBlock( &sz, sizeof( uint32_t ) );
                                chunks.addToBlock( i->second.data(), sz );
                            }
                        }
                        catch( std::exception & e )
                        {
                            qWarning() << "Error reading abrv file " << abrvFileName.c_str()
                                       << " Skipping it. " << e.what();
                        }
                    }

                    bool hasString = false;
                    wstring curString;
                    size_t curOffset = 0;

                    uint32_t articleCount = 0, wordCount = 0;

                    for( ; ; )
                    {
                        // Find the main headword

                        if ( !hasString && !scanner.readNextLine( curString, curOffset ) )
                            break; // Clean end of file

                        hasString = false;

                        // The line read should either consist of pure whitespace, or be a
                        // headword

                        if ( curString.empty() )
                            continue;

                        if ( isDslWs( curString[ 0 ] ) )
                        {
                            // The first character is blank. Let's make sure that all other
                            // characters are blank, too.
                            for( size_t x = 1; x < curString.size(); ++x )
                            {
                                if ( !isDslWs( curString[ x ] ) )
                                {
                                    qWarning() << "Garbage string in " << fName.c_str()
                                               << " at offset " << QString::number(curOffset,16);
                                    break;
                                }
                            }
                            continue;
                        }

                        // Ok, got the headword

                        list< wstring > allEntryWords;

                        processUnsortedParts( curString, true );
                        expandOptionalParts( curString, allEntryWords );

                        uint32_t articleOffset = curOffset;

                        //printf( "Headword: %ls\n", curString.c_str() );

                        // More headwords may follow

                        for( ; ; )
                        {
                            if ( ! ( hasString = scanner.readNextLine( curString, curOffset ) ) )
                            {
                                qWarning() << "Premature end of file " << fName.c_str();
                                break;
                            }

                            if ( curString.empty() || isDslWs( curString[ 0 ] ) )
                                break; // No more headwords

                            //printf( "Alt headword: %ls\n", curString.c_str() );

                            processUnsortedParts( curString, true );
                            expandTildes( curString, allEntryWords.front() );
                            expandOptionalParts( curString, allEntryWords );
                        }

                        if ( !hasString )
                            break;

                        // Insert new entry

                        uint32_t descOffset = chunks.startNewBlock();

                        chunks.addToBlock( &articleOffset, sizeof( articleOffset ) );

                        for( auto & word : allEntryWords )
                        {
                            unescapeDsl( word );
                            normalizeHeadword( word );
                            indexedWords.addWord( word, descOffset );
                        }

                        ++articleCount;
                        wordCount += allEntryWords.size();

                        // Skip the article's body
                        for( ; ; )
                        {
                            if ( ! ( hasString = scanner.readNextLine( curString, curOffset ) ) )
                                break;

                            if ( !curString.empty() && !isDslWs( curString[ 0 ] ) )
                                break;
                        }

                        // Now that we're having read the first string after the article
                        // itself, we can use its offset to calculate the article's size.
                        // An end of file works here, too.

                        uint32_t articleSize = ( curOffset - articleOffset );

                        chunks.addToBlock( &articleSize, sizeof( articleSize ) );

                        if ( !hasString )
                            break;
                    }

                    // Finish with the chunks

                    idxHeader.chunksOffset = chunks.finish();

                    // Build index

                    IndexInfo idxInfo = BtreeIndexing::buildIndex( indexedWords, idx );

                    idxHeader.indexBtreeMaxElements = idxInfo.btreeMaxElements;
                    idxHeader.indexRootOffset = idxInfo.rootOffset;

                    indexedWords.clear(); // Release memory -- no need for this data

                    // If there was a zip file, index it too

                    if ( !zipFileName.empty() )
                    {
                        //printf( "Indexing zip file\n" );

                        idxHeader.hasZipFile = 1;

                        QFile zipFile( QDir::fromNativeSeparators(
                                           QFile::decodeName( zipFileName.c_str() ) ) );

                        if ( !zipFile.open( QFile::ReadOnly ) )
                            throw exCantReadFile( zipFileName );

                        if ( ZipFile::positionAtCentralDir( zipFile ) )
                        {
                            // File seems to be a valid zip file

                            IndexedWords zipFileNames;

                            ZipFile::CentralDirEntry entry;

                            while( ZipFile::readNextEntry( zipFile, entry ) )
                            {
                                if ( entry.compressionMethod == ZipFile::Unsupported )
                                {
                                    qWarning() << "Compression method unsupported -- skipping file "
                                               << entry.fileName;
                                    continue;
                                }

                                // Check if the file name has some non-ascii letters.

                                auto ptr = reinterpret_cast<unsigned char const *>(
                                               entry.fileName.constData());

                                bool hasNonAscii = false;

                                for( ; ; )
                                {
                                    if ( *ptr & 0x80 )
                                    {
                                        hasNonAscii = true;
                                        break;
                                    }
                                    if ( !*ptr++ )
                                        break;
                                }

                                if ( !hasNonAscii )
                                {
                                    // Add entry as is

                                    zipFileNames.addSingleWord( Utf8::decode( entry.fileName.constData() ),
                                                                entry.localHeaderOffset );
                                }
                                else
                                {
                                    // Try assuming different encodings. Those are UTF8 and two
                                    // Russian ones (Windows and Windows OEM). Unfortunately, zip
                                    // files do not say which encoding they utilize.

                                    // Utf8
                                    try
                                    {
                                        wstring decoded = Utf8::decode( entry.fileName.constData() );

                                        zipFileNames.addSingleWord( decoded,
                                                                    entry.localHeaderOffset );
                                    }
                                    catch( Utf8::exCantDecode & )
                                    {
                                        // Failed to decode
                                    }

                                    // CP866
                                    try
                                    {
                                        wstring decoded = Iconv::toWstring( "CP866",
                                                                            entry.fileName.constData(),
                                                                            entry.fileName.size() );

                                        zipFileNames.addSingleWord( decoded,
                                                                    entry.localHeaderOffset );
                                    }
                                    catch( Iconv::Ex & )
                                    {
                                        // Failed to decode
                                    }

                                    // CP1251
                                    try
                                    {
                                        wstring decoded = Iconv::toWstring( "CP1251",
                                                                            entry.fileName.constData(),
                                                                            entry.fileName.size() );

                                        zipFileNames.addSingleWord( decoded,
                                                                    entry.localHeaderOffset );
                                    }
                                    catch( Iconv::Ex & )
                                    {
                                        // Failed to decode
                                    }

                                    // SJIS
                                    try
                                    {
                                        wstring decoded = Iconv::toWstring( "SJIS",
                                                                            entry.fileName.constData(),
                                                                            entry.fileName.size() );

                                        zipFileNames.addSingleWord( decoded,
                                                                    entry.localHeaderOffset );
                                    }
                                    catch( Iconv::Ex & )
                                    {
                                        // Failed to decode
                                    }

                                }
                            }

                            // Build the resulting zip file index

                            IndexInfo idxInfo = BtreeIndexing::buildIndex( zipFileNames, idx );

                            idxHeader.zipIndexBtreeMaxElements = idxInfo.btreeMaxElements;
                            idxHeader.zipIndexRootOffset = idxInfo.rootOffset;
                        }
                        else
                        {
                            // Bad zip file -- no index (though the mark that we have one
                            // remains)
                            idxHeader.zipIndexBtreeMaxElements = 0;
                            idxHeader.zipIndexRootOffset = 0;
                        }
                    }
                    else
                        idxHeader.hasZipFile = 0;

                    // That concludes it. Update the header.

                    idxHeader.signature = Signature;
                    idxHeader.formatVersion = CurrentFormatVersion;
                    idxHeader.zipSupportVersion = CurrentZipSupportVersion;

                    idxHeader.articleCount = articleCount;
                    idxHeader.wordCount = wordCount;

                    idxHeader.langFrom = dslLanguageToId( scanner.getLangFrom() );
                    idxHeader.langTo = dslLanguageToId( scanner.getLangTo() );

                    idx.rewind();

                    idx.write( &idxHeader, sizeof( idxHeader ) );

                } // In-place try for saving line count
                catch( ... )
                {
                    atLine = scanner.getLinesRead();
                    throw;
                }

            } // if need to rebuild

            dictionaries.push_back( new DslDictionary( dictId,
                                                       indexFile,
                                                       dictFiles ) );
        }
        catch( std::exception & e )
        {
            qWarning() << "DSL dictionary reading failed: " << fName.c_str()
                       << " :" << atLine << ", error: " << e.what();
        }
    }

    return dictionaries;
}


}
