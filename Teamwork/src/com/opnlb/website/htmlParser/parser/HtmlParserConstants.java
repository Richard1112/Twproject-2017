package com.opnlb.website.htmlParser.parser;


public interface HtmlParserConstants {

  int EOF = 0;

  int ALPHA_CHAR = 1;

  int NUM_CHAR = 2;

  int ALPHANUM_CHAR = 3;

  int IDENTIFIER_CHAR = 4;

  int IDENTIFIER = 5;

  int QUOTED_STRING_NB = 6;

  int QUOTED_STRING = 7;

  int WHITESPACE = 8;

  int NEWLINE = 9;

  int QUOTE = 10;

  int EOL = 11;

  int TAG_START = 12;

  int ENDTAG_START = 13;

  int COMMENT_START = 14;

  int DECL_START = 15;

  int PCDATA = 16;

  int PCDATA_QS = 17;

  int PCDATA_Q = 18;

  int TAG_SCRIPT = 19;

  int TAG_NAME = 20;

  int LST_ERROR = 21;

  int ATTR_NAME = 23;

  int TAG_END = 24;

  int ATTR_EQ = 25;

  int IMPLICIT_TAG_END = 26;

  int LIT_ERROR = 27;

  int ATTR_VAL = 29;

  int LAV_ERROR = 30;

  int COMMENT_END = 31;

  int DASH = 32;

  int COMMENT_EOL = 33;

  int COMMENT_WORD = 34;

  int DECL_ANY = 35;

  int DECL_END = 36;

  int SCRIPT_END = 37;

  int SCRIPT_EOL = 38;

  int SCRIPT_LBR = 39;

  int SCRIPT_WORD = 40;



  int LexScript = 0;

  int LexDecl = 1;

  int LexComment = 2;

  int LexAttrVal = 3;

  int LexInTag = 4;

  int LexStartTag = 5;

  int DEFAULT = 6;



  String[] tokenImage = {

    "<EOF>",

    "<ALPHA_CHAR>",

    "<NUM_CHAR>",

    "<ALPHANUM_CHAR>",

    "<IDENTIFIER_CHAR>",

    "<IDENTIFIER>",

    "<QUOTED_STRING_NB>",

    "<QUOTED_STRING>",

    "<WHITESPACE>",

    "<NEWLINE>",

    "<QUOTE>",

    "<EOL>",

    "\"<\"",

    "\"</\"",

    "\"<!--\"",

    "\"<!\"",

    "<PCDATA>",

    "<PCDATA_QS>",

    "<PCDATA_Q>",

    "\"SCRIPT\"",

    "<TAG_NAME>",

    "<LST_ERROR>",

    "<token of kind 22>",

    "<ATTR_NAME>",

    "\">\"",

    "\"=\"",

    "\"<\"",

    "<LIT_ERROR>",

    "<token of kind 28>",

    "<ATTR_VAL>",

    "<LAV_ERROR>",

    "\"-->\"",

    "\"-\"",

    "<COMMENT_EOL>",

    "<COMMENT_WORD>",

    "<DECL_ANY>",

    "\">\"",

    "\"</SCRIPT>\"",

    "<SCRIPT_EOL>",

    "\"<\"",

    "<SCRIPT_WORD>",

  };



}

