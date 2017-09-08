package org.jblooming.utilities.file;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Writer;

public class CSVPrinterExtended {

  protected char delimiterChar = ',';

  protected char quoteChar = '"';

  protected PrintWriter out;

  protected boolean newLine = true;

  protected char commentStart = '#';

  private boolean quoteDefault=false;  //if true everithing is quoted

  public void changeDelimiter(char newDelimiter) throws Exception {
    if (delimiterChar == newDelimiter) return; // no need to do anything.
    if (newDelimiter == '\n' || newDelimiter == '\r' ||
            newDelimiter == delimiterChar || newDelimiter == quoteChar) {
      throw new Exception();
    }
    delimiterChar = newDelimiter;
  }

  public void changeQuote(char newQuote) throws Exception {
    if (newQuote == '\n' || newQuote == '\r' ||
            newQuote == delimiterChar || newQuote == quoteChar) {
      throw new Exception();
    }
    quoteChar = newQuote;
  }

  public CSVPrinterExtended(OutputStream out) {
    this.out = new PrintWriter(out);
  }

  public CSVPrinterExtended(Writer out) {
    if (out instanceof PrintWriter) {
      this.out = (PrintWriter) out;
    } else {
      this.out = new PrintWriter(out);
    }
  }

  public CSVPrinterExtended(OutputStream out, char commentStart) {
    this(out);
    this.commentStart = commentStart;
  }

  public CSVPrinterExtended(Writer out, char commentStart) {
    this(out);
    this.commentStart = commentStart;
  }

  public void println(String value) {
    print(value);
    out.println();
    out.flush();
    newLine = true;
  }

  public void println() {
    out.println();
    out.flush();
    newLine = true;
  }

  public void println(String[] values) {
    for (int i = 0; i < values.length; i++) {
      print(values[i]);
    }
    out.println();
    out.flush();
    newLine = true;
  }

  public void println(String[][] values) {
    for (int i = 0; i < values.length; i++) {
      println(values[i]);
    }
    if (values.length == 0) {
      out.println();
    }
    out.flush();
    newLine = true;
  }

  public void printlnComment(String comment) {
    if (comment == null) comment = "";
    if (!newLine) {
      out.println();
    }
    out.print(commentStart);
    out.print(' ');
    for (int i = 0; i < comment.length(); i++) {
      char c = comment.charAt(i);
      switch (c) {
        case '\r': {
          if (i + 1 < comment.length() && comment.charAt(i + 1) == '\n') {
            i++;
          }
        } //break intentionally excluded.
        case '\n': {
          out.println();
          out.print(commentStart);
          out.print(' ');
        }
        break;
        default: {
          out.print(c);
        }
        break;
      }
    }
    out.println();
    out.flush();
    newLine = true;
  }

  public void print(String value) {
    print(value,false);
  }

  public void print(String value,boolean forceQuoted) {
    if (value == null)
      value = "";
    boolean quote = false;
    quote = forceQuoted || quoteDefault ;
    if (value.length() > 0) {

      char c = value.charAt(0);
      if (newLine && (c < '0' || (c > '9' && c < 'A') || (c > 'Z' && c < 'a') || (c > 'z'))) {
        quote = true;
      }
      if (c == ' ' || c == '\f' || c == '\t') {
        quote = true;
      }
      for (int i = 0; i < value.length(); i++) {
        c = value.charAt(i);
        if (c == quoteChar || c == delimiterChar || c == '\n' || c == '\r') {
          quote = true;
        }
      }
      if (c == ' ' || c == '\f' || c == '\t') {
        quote = true;
      }
    } else if (newLine) {
      // always quote an empty token that is the firs
      // on the line, as it may be the only thing on the
      // line.  If it were not quoted in that case,
      // an empty line has no tokens.
      quote = true;
    }
    if (newLine) {
      newLine = false;
    } else {
      out.print(delimiterChar);
    }
    if (quote) {
      out.print(escapeAndQuote(value));
    } else {
      out.print(value);
    }
    out.flush();
  }

  private String escapeAndQuote(String value) {
    int count = 2;
    for (int i = 0; i < value.length(); i++) {
      char c = value.charAt(i);
      switch (c) {
        case '\n':
        case '\r':
        case '\\': {
          count++;
        }
        break;
        default: {
          if (c == quoteChar) {
            count++;
          }
        }
        break;
      }
    }
    StringBuffer sb = new StringBuffer(value.length() + count);
    sb.append(quoteChar);
    for (int i = 0; i < value.length(); i++) {
      char c = value.charAt(i);
      switch (c) {
        case '\n': {
          sb.append("\\n");
        }
        break;
        case '\r': {
          sb.append("\\r");
        }
        break;
        case '\\': {
          sb.append("\\\\");
        }
        break;
        default: {
          if (c == quoteChar) {
            sb.append("\\" + quoteChar);
          } else {
            sb.append(c);
          }
        }
      }
    }
    sb.append(quoteChar);
    return (sb.toString());
  }


  public void setQuoteDefault(boolean def){
    quoteDefault=def;
  }

}

