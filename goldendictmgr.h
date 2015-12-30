#ifndef GOLDENDICTMGR_H
#define GOLDENDICTMGR_H

#include <QObject>
#include <QStringList>
#include <QList>
#include <QMap>
#include <QPair>
#include <QWidget>
#include <QNetworkAccessManager>
#include <QUrl>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QIODevice>
#include <QThread>

#include <vector>
#include <set>
#include <string>
#include "wstring.hh"
#include "sptr.hh"
#include "dictionary.hh"
#include "wordfinder.hh"

class CGlobalControl;

class CDictLoader : public QThread, public Dictionary::Initializing
{
    Q_OBJECT

private:
    QStringList nameFilters;
    QStringList paths;
    std::vector< sptr< Dictionary::Class > > dictionaries;
    std::string exceptionText;

public:
    CDictLoader(QObject * parent, const QStringList& dictPaths);
    virtual void run();
    std::vector< sptr< Dictionary::Class > > const & getDictionaries() const
    { return dictionaries; }
    std::string const & getExceptionText() const
    { return exceptionText; }

signals:
    void indexingDictionarySignal( QString const & dictionaryName );

public:
    virtual void indexingDictionary( std::string const & dictionaryName ) throw();

private:
    void handlePath( const QString& path, bool recursive );
};

class ArticleRequest: public Dictionary::DataRequest
{
  Q_OBJECT

  QString word, group;
  QMap< QString, QString > contexts;
  std::vector< sptr< Dictionary::Class > > activeDicts;

  std::set< gd::wstring > alts; // Accumulated main forms
  std::list< sptr< Dictionary::WordSearchRequest > > altSearches;
  bool altsDone, bodyDone;
  std::list< sptr< Dictionary::DataRequest > > bodyRequests;
  bool foundAnyDefinitions;
  bool closePrevSpan; // Indicates whether the last opened article span is to
                      // be closed after the article ends.
  sptr< WordFinder > stemmedWordFinder; // Used when there're no results

  /// A sequence of words and spacings between them, including the initial
  /// spacing before the first word and the final spacing after the last word.
  typedef QList< QString > Words;
  typedef QList< QString > Spacings;

  /// Splits the given string into words and spacings between them.
  QPair< Words, Spacings > splitIntoWords( QString const & );

  QPair< Words, Spacings > splittedWords;
  int currentSplittedWordStart;
  int currentSplittedWordEnd;
  QString currentSplittedWordCompound;
  QString lastGoodCompoundResult;
  bool firstCompoundWasFound;

public:

  ArticleRequest( QString const & word, QString const & group,
                  QMap< QString, QString > const & contexts,
                  std::vector< sptr< Dictionary::Class > > const & activeDicts,
                  std::string const & header );

  virtual void cancel()
  { finish(); } // Add our own requests cancellation here

private slots:

  void altSearchFinished();
  void bodyFinished();
  void stemmedSearchFinished();
  void individualWordFinished();

private:

  /// Appends the given string to 'data', with locking its mutex.
  void appendToData( std::string const & );

  /// Uses stemmedWordFinder to perform the next step of looking up word
  /// combinations.
  void compoundSearchNextStep( bool lastSearchSucceeded );

  /// Creates a single word out of the [currentSplittedWordStart..End] range.
  QString makeSplittedWordCompound();

  /// Makes an html link to the given word.
  std::string linkWord( QString const & );

  /// Escapes the spacing between the words to include in html.
  std::string escapeSpacing( QString const & );
};

class CGoldenDictMgr : public QObject
{
    Q_OBJECT
public:
    std::vector< sptr< Dictionary::Class > > dictionaries;

    explicit CGoldenDictMgr(QObject *parent = 0);

    sptr< Dictionary::DataRequest > makeDefinitionFor( QString const & word,
                                                       QMap< QString, QString > const & contexts ) const;

    sptr< Dictionary::DataRequest > makeNotFoundTextFor( QString const & word ) const;

    /// Creates an 'untitled' page. The result is guaranteed to be instant.
    sptr< Dictionary::DataRequest > makeEmptyPage() const;

    QStringList getLoadedDictionaries();

private:
    std::string makeHtmlHeader( QString const & word ) const;
    static std::string makeNotFoundBody( QString const & word );

    friend class ArticleRequest;
    friend class ArticleNetworkAccessManager;

signals:

public slots:
    void loadDictionaries();
    void loadDone();
    void showMessage(const QString& msg);

};

class ArticleNetworkAccessManager: public QNetworkAccessManager
{
  CGoldenDictMgr * dictMgr;

public:

  ArticleNetworkAccessManager( QObject * parent,
                               CGoldenDictMgr * dictMgr_):
    QNetworkAccessManager( parent ),
    dictMgr( dictMgr_ )
  {}

  /// Tries handling any kind of internal resources referenced by dictionaries.
  /// If it succeeds, the result is a dictionary request object. Otherwise, an
  /// empty pointer is returned.
  /// The function can optionally set the Content-Type header correspondingly.
  sptr< Dictionary::DataRequest > getResource( QUrl const & url,
                                               QString & contentType );

protected:

  virtual QNetworkReply * createRequest( Operation op,
                                         QNetworkRequest const & req,
                                         QIODevice * outgoingData );
};

class ArticleResourceReply: public QNetworkReply
{
  Q_OBJECT

  sptr< Dictionary::DataRequest > req;
  size_t alreadyRead;

public:

  ArticleResourceReply( QObject * parent,
                        QNetworkRequest const &,
                        sptr< Dictionary::DataRequest > const &,
                        QString const & contentType );

  ~ArticleResourceReply();

protected:

  virtual qint64 bytesAvailable() const;

  virtual void abort()
  {}
  virtual qint64 readData( char * data, qint64 maxSize );

  // We use the hackery below to work around the fact that we need to emit
  // ready/finish signals after we've been constructed.
signals:

  void readyReadSignal();
  void finishedSignal();

private slots:

  void reqUpdated();
  void reqFinished();

  void readyReadSlot();
  void finishedSlot();
};

class BlockedNetworkReply: public QNetworkReply
{
  Q_OBJECT

public:

  BlockedNetworkReply( QObject * parent );

  virtual qint64 readData( char *, qint64 )
  {
    return -1;
  }

  virtual void abort()
  {}

protected:

  // We use the hackery below to work around the fact that we need to emit
  // ready/finish signals after we've been constructed.

signals:

  void finishedSignal();

private slots:

  void finishedSlot();
};


#endif // GOLDENDICTMGR_H
