QT += network xml
QT -= gui

# warn on *any* usage of deprecated APIs
DEFINES += QT_DEPRECATED_WARNINGS
# ... and just fail to compile if APIs deprecated in Qt <= 5.10 are used
DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x050A00
CONFIG += warn_on c++14

TARGET = goldendict
TEMPLATE = lib
DEFINES += GOLDENDICT_LIBRARY

SOURCES += \
    goldendictmgr.cpp \
    zipfile.cc \
    wstring_qt.cc \
    wstring.cc \
    utf8.cc \
    stardict.cc \
    mutex.cc \
    langcoder.cc \
    folding.cc \
    dsl_details.cc \
    dsl.cc \
    dictzip.c \
    indexedzip.cc \
    iconv.cc \
    htmlescape.cc \
    fsencoding.cc \
    dictionary.cc \
    dictdfiles.cc \
    chunkedstorage.cc \
    btreeidx.cc \
    audiolink.cc \
    xdxf2html.cc \
    file.cc \
    filetype.cc \
    wordfinder.cc \
    romaji.cc \
    transliteration.cc

HEADERS += \
    zipfile.hh \
    xdxf2html.hh \
    wstring_qt.hh \
    wstring.hh \
    utf8.hh \
    stardict.hh \
    sptr.hh \
    mutex.hh \
    langcoder.hh \
    ex.hh \
    dsl_details.hh \
    dsl.hh \
    dictzip.h \
    dictionary.hh \
    indexedzip.hh \
    iconv.hh \
    htmlescape.hh \
    fsencoding.hh \
    folding.hh \
    dictdfiles.hh \
    chunkedstorage.hh \
    btreeidx.hh \
    audiolink.hh \
    file.hh \
    inc_diacritic_folding.hh \
    inc_case_folding.hh \
    filetype.hh \
    wordfinder.hh \
    romaji.hh \
    transliteration.hh \
    goldendict_global.hh \
    goldendictmgr.hh

unix {
    target.path = /usr/lib
    headers.path = /usr/include/goldendictlib
    headers.files = $$HEADERS
    INSTALLS += target headers
}
