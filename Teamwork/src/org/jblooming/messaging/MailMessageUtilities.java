package org.jblooming.messaging;

import org.jblooming.tracer.Tracer;

import javax.mail.*;
import javax.mail.internet.*;
import javax.activation.*;
import java.io.*;
import java.util.Enumeration;
import java.util.Vector;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Apr 10, 2007
 * Time: 6:09:35 PM
 */
public class MailMessageUtilities {

  /**
   * This wrapper is used to work around a bug in the sun io inside
   * sun.io.ByteToCharConverter.getConverterClass().
   * <p/>
   * The programmer uses a cute coding trick
   * that appends the charset to a prefix to create a Class name that
   * they then attempt to load. The problem is that if the charset is
   * not one that they "recognize", and the name has something like a
   * dash character in it, the resulting Class name has an invalid
   * character in it. This results in an IllegalArgumentException
   * instead of the UnsupportedEncodingException that is documented.
   * Thus, we need to catch the undocumented exception.
   *
   * @param part The part from which to get the content.
   * @return The content.
   * @throws MessagingException if the content charset is unsupported.
   */
  public static Object getPartContent(Part part) throws MessagingException {

    Object result = null;

    try {
      result = part.getContent();
    } catch (IllegalArgumentException ex) {
      throw new MessagingException("content charset is not recognized: " + ex.getMessage());
    } catch (IOException ex) {
      throw new MessagingException("getPartContent(): " + ex.getMessage());
    }

    return result;
  }

  /**
   * This wrapper is used to work around a bug in the sun io inside
   * sun.io.ByteToCharConverter.getConverterClass().
   * <p/>
   * The programmer uses a cute coding trick
   * that appends the charset to a prefix to create a Class name that
   * they then attempt to load. The problem is that if the charset is
   * not one that they "recognize", and the name has something like a
   * dash character in it, the resulting Class name has an invalid
   * character in it. This results in an IllegalArgumentException
   * instead of the UnsupportedEncodingException that is documented.
   * Thus, we need to catch the undocumented exception.
   *
   * @param dh The DataHandler from which to get the content.
   * @return The content.
   * @throws MessagingException if the content charset is unsupported.
   */
  public static Object getDataHandlerContent(DataHandler dh) throws MessagingException {

    Object result = null;

    try {
      result = dh.getContent();
    } catch (IllegalArgumentException ex) {
      throw new MessagingException("content charset is not recognized: " + ex.getMessage());
    } catch (IOException ex) {
      throw new MessagingException("getDataHandlerContent(): " + ex.getMessage());
    }

    return result;
  }

  /**
   * Create a reply message to the given message.
   * The reply message is addressed to only the from / reply address or
   * all receipients based on the replyToAll flag.
   *
   * @param message The message which to reply
   * @param body The attached text to include in the reply
   * @param replyToAll Reply to all receipients of the original message
   * @return Message Reply message
   * @exception MessagingException if the message contents are invalid
   */
  /*
 public static Message createReply(Message message, String body, boolean replyToAll) throws MessagingException {
   // create an empty reply message
   Message xreply = message.reply(replyToAll);

   // set the default from address
   xreply.setFrom(getDefaultFromAddress());

   // UNDONE should any extra headers be replied to?

   if (message instanceof MimeMessage) {
     ((MimeMessage) xreply).setText(body, "UTF-8");
   } else {
     xreply.setText(body);
   }

   // Message.reply() may set the "replied to" flag, so
   // attempt to save that new state and fail silently...
   try {
     message.saveChanges();
   } catch (MessagingException ex) {
   }

   return xreply;
 }
  */
  /**
   * Create a forward message to the given message.
   * The forward message addresses no receipients but
   * contains all the contents of the given message.
   * Another missing method in JavaMail!!!
   *
   * @param message The message which to forward
   * @return Message Forwarding message
   * @exception MessagingException if the message contents are invalid
   */

  /*
public static Message createForward(Message message) throws MessagingException {
// create an initial message without addresses, subject, or content
MimeMessage xforward = new MimeMessage(ICEMail.getDefaultSession());

// create forwarding headers
InternetHeaders xheaders = getHeaders(message);
xheaders.removeHeader("from");
xheaders.removeHeader("to");
xheaders.removeHeader("cc");
xheaders.removeHeader("bcc");
xheaders.removeHeader("received");
addHeaders(xforward, xheaders);

// set the default from address
xforward.setFrom(getDefaultFromAddress());

// create the subject
String xsubject = message.getSubject().trim();
if (xsubject.toUpperCase().indexOf("FW") != 0) {
  xsubject = "FW: " + xsubject;
}

xforward.setSubject(xsubject, "UTF-8");

// create the contents
ContentType xctype = getContentType(message);

Object xcont = getPartContent(message);

xforward.setContent(xcont, xctype.toString());

// REVIEW - why would we need to save here?!
//  xforward.saveChanges();

return xforward;
}    */

//............................................................

  /**
   * Decode from address(es) of the message into UTF strings.
   * This is a convenience method provided to simplify application code.
   *
   * @param message The message to interogate
   * @return String List of decoded addresses
   * @throws MessagingException if the message contents are invalid
   */
  public static String decodeFrom(javax.mail.Message message) throws MessagingException {

    Address[] xaddresses = message.getFrom();
    return decodeAddresses(xaddresses);
  }

  /**
   * Decode recipent addresses of the message into UTF strings.
   * This is a convenience method provided to simplify application code.
   *
   * @param message The message to interogate
   * @param type    The type of message recipients to decode, i.e. from, to, cc, etc
   * @return String List of decoded addresses
   * @throws MessagingException if the message contents are invalid
   */
  public static String
  decodeAddresses(javax.mail.Message message, javax.mail.Message.RecipientType type) throws MessagingException {
    Address[] xaddresses = message.getRecipients(type);
    return decodeAddresses(xaddresses);
  }

  /**
   * Decode mail addresses into UTF strings.
   * <p/>
   * This routine is necessary because Java Mail Address.toString() routines
   * convert into MIME encoded strings (ASCII C and B encodings), not UTF.
   * Of course, the returned string is in the same format as MIME, but converts
   * to UTF character encodings.
   *
   * @param addresses The list of addresses to decode
   * @return String List of decoded addresses
   */
  public static String decodeAddresses(Address[] addresses) {

    StringBuffer xlist = new StringBuffer();

    if (addresses != null) {
      for (int xindex = 0; xindex < addresses.length; xindex++) {
        // at this time, only internet addresses can be decoded
        if (xlist.length() > 0)
          xlist.append(", ");

        if (addresses[xindex] instanceof InternetAddress) {
          InternetAddress xinet = (InternetAddress) addresses[xindex];

          if (xinet.getPersonal() == null) {
            xlist.append(xinet.getAddress());
          } else {
            // If the address has a ',' in it, we must
            // wrap it in quotes, or it will confuse the
            // code that parses addresses separated by commas.
            String personal = xinet.getPersonal();
            int idx = personal.indexOf(",");
            String qStr = (idx == -1 ? "" : "\"");
            xlist.append(qStr);
            xlist.append(personal);
            xlist.append(qStr);
            xlist.append(" <");
            xlist.append(xinet.getAddress());
            xlist.append(">");
          }
        } else {
          // generic, and probably not portable,
          // but what's a man to do...
          xlist.append(addresses[xindex].toString());
        }
      }
    }

    return xlist.toString();
  }

  /**
   * Encode UTF strings into mail addresses.
   */
  public static InternetAddress[] encodeAddresses(String string, String charset) throws MessagingException {

    // parse the string into the internet addresses
    // NOTE: these will NOT be character encoded
    InternetAddress[] xaddresses = InternetAddress.parse(string);

    // now encode each to the given character set
    for (int xindex = 0; xindex < xaddresses.length; xindex++) {
      String xpersonal = xaddresses[xindex].getPersonal();
      try {
        if (xpersonal != null) {
          xaddresses[xindex].setPersonal(xpersonal, charset);
        }
      } catch (UnsupportedEncodingException xex) {
        throw new MessagingException(xex.toString());
      }
    }

    return xaddresses;
  }
/*
    public static InternetAddress getDefaultFromAddress() throws MessagingException {
      // encode the address
      String xcharset = "UTF-8";
      InternetAddress[] xaddresses = encodeAddresses(getDefaultFromString(), xcharset);

      return xaddresses[0];
    }

    public static String getDefaultFromString() {

      // build a string from the user defined From personal and address
      String xstring = UserProperties.getProperty("fromPersonal", null);

      String xaddress = UserProperties.getProperty("fromAddress", null);

      if (xaddress == null) {
        xaddress = UserProperties.getProperty(".user.name", "unknown");
      }

      if (xstring != null && xstring.length() > 0) {
        xstring = xstring + " <" + xaddress + ">";
      } else {
        xstring = xaddress;
      }

      return xstring;
    }

    public static InternetAddress getDefaultReplyTo() throws MessagingException {
      // build a string from the user defined ReplyTo personal and address
      String xstring = UserProperties.getProperty("replyToPersonal", null);

      String xaddress = UserProperties.getProperty("replyToAddress", null);

      if (xaddress == null) {
        xaddress = UserProperties.getProperty(".user.name", "unknown");
      }

      if (xstring != null && xstring.length() > 0) {
        xstring = xstring + " <" + xaddress + ">";
      } else {
        xstring = xaddress;
      }

      // encode the address
      String xcharset = "UTF-8";
      InternetAddress[] xaddresses = encodeAddresses(xstring, xcharset);

      return xaddresses[0];
    }
    */
//............................................................

  public static InternetHeaders getHeadersWithFrom(javax.mail.Message message) throws MessagingException {
    Header xheader;
    InternetHeaders xheaders = new InternetHeaders();
    Enumeration xe = message.getAllHeaders();
    for (; xe.hasMoreElements();) {
      xheader = (Header) xe.nextElement();
      xheaders.addHeader(xheader.getName(), xheader.getValue());
    }

    return xheaders;
  }

  public static InternetHeaders getHeaders(javax.mail.Message message) throws MessagingException {
    Header xheader;
    InternetHeaders xheaders = new InternetHeaders();
    Enumeration xe = message.getAllHeaders();
    for (; xe.hasMoreElements();) {
      xheader = (Header) xe.nextElement();
      if (!xheader.getName().startsWith("From ")) {
        xheaders.addHeader(xheader.getName(), xheader.getValue());
      }
    }

    return xheaders;
  }

  public static void addHeaders(javax.mail.Message message, InternetHeaders headers) throws MessagingException {
    Header xheader;
    Enumeration xe = headers.getAllHeaders();
    for (; xe.hasMoreElements();) {
      xheader = (Header) xe.nextElement();
      message.addHeader(xheader.getName(), xheader.getValue());
    }
  }



  public static void attach(MimeMultipart multipart, File file, String charset) throws MessagingException {

    // UNDONE how to specify the character set of the file???
    MimeBodyPart xbody = new MimeBodyPart();
    FileDataSource xds = new FileDataSource(file);
    DataHandler xdh = new DataHandler(xds);
    xbody.setDataHandler(xdh);

    // UNDONE
    // xbody.setContentLanguage( String ); // this could be language from Locale
    // xbody.setContentMD5( String md5 ); // don't know about this yet
    xbody.setDescription("File Attachment: " + file.getName(), charset);
    xbody.setDisposition(Part.ATTACHMENT);
    setFileName(xbody, file.getName(), charset);

    multipart.addBodyPart(xbody);
  }

  public static void attach(MimeMultipart multipart, String text, String charset, String disposition) throws MessagingException {

    int xid = multipart.getCount() + 1;
    String xname = "TEXT" + xid + ".TXT";

    MimeBodyPart xbody = new MimeBodyPart();
    xbody.setText(text, charset);

    // UNDONE
    //xbody.setContentLanguage( String ); // this could be language from Locale
    //xbody.setContentMD5( String md5 ); // don't know about this yet
    xbody.setDescription("Text Attachment: " + xname, charset);
    xbody.setDisposition(disposition);
    setFileName(xbody, xname, charset);

    multipart.addBodyPart(xbody);
  }

//............................................................

  /**
   * Decode the contents of the Part into text and attachments.
   */
  public static StringBuffer decodeContent(Part part, StringBuffer buffer, Vector attachments, Vector names) throws MessagingException {

    subDecodeContent(part, buffer, attachments, names);

    // If we did not get any body text, scan on more time
    // for a text/plain part that is not 'inline', and use
    // that as a proxy...

    if (buffer.length() == 0 && attachments != null) {
      for (int i = 0, sz = attachments.size(); i < sz; ++i) {
        Part p = (Part) attachments.elementAt(i);

        ContentType xctype = getContentType(p);

        if (xctype.match("text/plain")) {
          decodeTextPlain(buffer, p);

          attachments.removeElementAt(i);
          if (names != null) {
            names.removeElementAt(i);
          }

          break;
        }
      }
    }

    return buffer;
  }

  /**
   * Given a message that we are replying to, or forwarding,
   *
   * @param part        The part to decode.
   * @param buffer      The new message body text buffer.
   * @param attachments Vector for new message's attachments.
   * @param names       Vector for new message's attachment descriptions.
   * @return The buffer being filled in with the body.
   */

  protected static StringBuffer subDecodeContent(Part part, StringBuffer buffer, Vector attachments, Vector names) throws MessagingException {

    boolean attachIt = true;

    // decode based on content type and disposition
    ContentType xctype = getContentType(part);

    ContentDisposition xcdisposition = getContentDisposition(part);

    if (xctype.match("multipart/*")) {
      attachIt = false;

      Multipart xmulti = (Multipart) getPartContent(part);

      int xparts = xmulti.getCount();

      for (int xindex = 0; xindex < xparts; xindex++) {
        subDecodeContent(xmulti.getBodyPart(xindex),
                buffer, attachments, names);
      }
    } else if (xctype.match("text/plain")) {
      //if (xcdisposition.equals(Part.INLINE)) {
      attachIt = false;
      decodeTextPlain(buffer, part);
      //}
    }

    if (attachIt) {
      // UNDONE should simple single line entries be
      //        created for other types and attachments?
      //
      // UNDONE should attachements only be created for "attachments" or all
      // unknown content types?

      if (attachments != null) {
        attachments.addElement(part);
      }

      if (names != null) {
        names.addElement(getPartName(part) +
                " (" + xctype.getBaseType() + ") " + part.getSize() + " bytes");
      }
    }

    return buffer;
  }

  /**
   * Get the name of a part.
   * The part is interogated for a valid name from the provided file name
   * or description.
   *
   * @param part The part to interogate
   * @return String containing the name of the part
   * @throws MessagingException if contents of the part are invalid
   * @see javax.mail.Part
   */
  public static String getPartName(Part part) throws MessagingException {

    String xdescription = getFileName(part);

    if (xdescription == null || xdescription.length() < 1) {
      xdescription = part.getDescription();
    }

    if ((xdescription == null || xdescription.length() < 1) && part instanceof MimePart) {
      xdescription = ((MimePart) part).getContentID();
    }

    if (xdescription == null || xdescription.length() < 1) {
      xdescription = "Message Part";
    }

    return xdescription;
  }

  /**
   * Decode contents of TEXT/PLAIN message parts into UTF encoded strings.
   * Why can't JAVA Mail provide this method?? Who knows!?!?!?
   */
  public static StringBuffer decodeTextPlain(StringBuffer buffer, Part part) throws MessagingException {
    // pick off the individual lines of text
    // and append to the buffer
    //
    BufferedReader xreader = getTextReader(part);
    return decodeTextPlain(buffer, xreader);
  }

  public static StringBuffer decodeTextPlain(StringBuffer buffer, BufferedReader xreader) throws MessagingException {
    // pick off the individual lines of text
    // and append to the buffer
    //
    try {
      for (String xline; (xline = xreader.readLine()) != null;) {
        buffer.append(xline + '\n');
      }

      xreader.close();
      return buffer;
    } catch (IOException xex) {
      throw new MessagingException(xex.toString());
    }
  }


  public static BufferedReader getTextReader(Part part) throws MessagingException {
    try {
      InputStream xis = part.getInputStream(); // transfer decoded only

      // pick the character set off of the content type
      ContentType xct = getContentType(part);
      String xjcharset = xct.getParameter("charset");
      if (xjcharset == null) {
        // not present, assume ASCII character encoding
        xjcharset = "ISO-8859-1"; // US-ASCII in JAVA terms
      }

      // now construct a reader from the decoded stream
      return getTextReader(xis, xjcharset);
    } catch (IOException xex) {
      throw new MessagingException(xex.toString());
    }
  }

  public static BufferedReader getTextReader(InputStream xis, ContentType xct) throws MessagingException {
    try {
      String xjcharset = xct.getParameter("charset");
      if (xjcharset == null) {
        // not present, assume ASCII character encoding
        xjcharset = "ISO-8859-1"; // US-ASCII in JAVA terms
      }

      // now construct a reader from the decoded stream
      return getTextReader(xis, xjcharset);
    } catch (IOException xex) {
      throw new MessagingException(xex.toString());
    }
  }


  public static BufferedReader getTextReader(InputStream stream, String charset) throws UnsupportedEncodingException {
    // Sun has a HUGE bug in their io code. They use a cute coding trick
    // that appends the charset to a prefix to create a Class name that
    // they then attempt to load. The problem is that if the charset is
    // not one that they "recognize", and the name has something like a
    // dash character in it, the resulting Class name has an invalid
    // character in it. This results in an IllegalArgumentException
    // instead of the UnsupportedEncodingException that we expect. Thus,
    // we need to catch the undocumented exception.

    InputStreamReader inReader;

    try {
      charset = MimeUtility.javaCharset(charset); // just to be sure
      inReader = new InputStreamReader(stream, charset);
    } catch (UnsupportedEncodingException ex) {
      inReader = null;
    } catch (IllegalArgumentException ex) {
      inReader = null;
    }

    if (inReader == null) {
      // This is the "bug" case, and we need to do something
      // that will at least put text in front of the user...
      inReader = new InputStreamReader(stream, "ISO-8859-1");
    }

    return new BufferedReader(inReader);
  }

//............................................................

  /**
   * Get a textual description of a message.
   * This is a helper method for applications.
   *
   * @param msg The message to interogate
   * @return String containing the description of the message
   */
  public static String getMessageDescription(javax.mail.Message msg) throws MessagingException {

    StringBuffer xbuffer = new StringBuffer(1024);
    getPartDescription(msg, xbuffer, "", true);
    return xbuffer.toString();
  }

  /**
   * Get a textual description of a part.
   *
   * @param part    The part to interogate
   * @param buf     a string buffer for the description
   * @param prefix  a prefix for each line of the description
   * @param recurse boolean specifying wether to recurse through sub-parts or not
   * @return StringBuffer containing the description of the part
   */
  public static StringBuffer getPartDescription(Part part, StringBuffer buf, String prefix, boolean recurse) throws MessagingException {

    if (buf == null)
      return buf;

    ContentType xctype = getContentType(part);

    String xvalue = xctype.toString();
    buf.append(prefix);
    buf.append("Content-Type: ");
    buf.append(xvalue);
    buf.append('\n');

    xvalue = part.getDisposition();
    buf.append(prefix);
    buf.append("Content-Disposition: ");
    buf.append(xvalue);
    buf.append('\n');

    xvalue = part.getDescription();
    buf.append(prefix);
    buf.append("Content-Description: ");
    buf.append(xvalue);
    buf.append('\n');

    xvalue = getFileName(part);
    buf.append(prefix);
    buf.append("Content-Filename: ");
    buf.append(xvalue);
    buf.append('\n');

    if (part instanceof MimePart) {
      MimePart xmpart = (MimePart) part;
      xvalue = xmpart.getContentID();
      buf.append(prefix);
      buf.append("Content-ID: ");
      buf.append(xvalue);
      buf.append('\n');

      String[] langs = xmpart.getContentLanguage();
      if (langs != null) {
        buf.append(prefix);
        buf.append("Content-Language: ");
        for (int pi = 0; pi < langs.length; ++pi) {
          if (pi > 0)
            buf.append(", ");
          buf.append(xvalue);
        }
        buf.append('\n');
      }

      xvalue = xmpart.getContentMD5();
      buf.append(prefix);
      buf.append("Content-MD5: ");
      buf.append(xvalue);
      buf.append('\n');

      xvalue = xmpart.getEncoding();
      buf.append(prefix);
      buf.append("Content-Encoding: ");
      buf.append(xvalue);
      buf.append('\n');
    }

    buf.append('\n');

    if (recurse && xctype.match("multipart/*")) {
      Multipart xmulti = (Multipart) getPartContent(part);

      int xparts = xmulti.getCount();
      for (int xindex = 0; xindex < xparts; xindex++) {
        getPartDescription(xmulti.getBodyPart(xindex),
                buf, (prefix + "   "), true);
      }
    }

    return buf;
  }

  /**
   * Get the content dispostion of a part.
   * The part is interogated for a valid content disposition. If the
   * content disposition is missing, a default disposition is created
   * based on the type of the part.
   *
   * @param part The part to interogate
   * @return ContentDisposition of the part
   * @see javax.mail.Part
   */
  public static ContentDisposition getContentDisposition(Part part) throws MessagingException {

    String xheaders[] = part.getHeader("Content-Disposition");

    try {
      if (xheaders != null) {
        return new ContentDisposition(xheaders[0]);
      }
    } catch (ParseException xex) {
      throw new MessagingException(xex.toString());
    }

    // set default disposition based on part type
    if (part instanceof MimeBodyPart) {
      return new ContentDisposition("attachment");
    }

    return new ContentDisposition("inline");
  }

  /**
   * A 'safe' version of JavaMail getContentType(), i.e. don't throw exceptions.
   * The part is interogated for a valid content type. If the content type is
   * missing or invalid, a default content type of "text/plain" is assumed,
   * which is suggested by the MIME standard.
   *
   * @param part The part to interogate
   * @return ContentType of the part
   * @see javax.mail.Part
   */
  public static ContentType getContentType(Part part) {

    String xtype = null;
    try {
      xtype = part.getContentType();
    } catch (MessagingException xex) {
    }

    return getContentType(xtype);
  }

  public static ContentType getContentType(String xtype) {

    if (xtype == null) {
      xtype = "text/plain"; // MIME default content type if missing
    }
    ContentType xctype = null;
    try {
      xctype = new ContentType(xtype.toLowerCase());
    } catch (ParseException xex) {
    }
    if (xctype == null) {
      xctype = new ContentType("text", "plain", null);
    }
    return xctype;
  }

  /**
   * Determin if the message is high-priority.
   *
   * @param message the message to examine
   * @return true if the message is high-priority
   */
  public static boolean isHighPriority(javax.mail.Message message) throws MessagingException {

    if (message instanceof MimeMessage) {
      MimeMessage xmime = (MimeMessage) message;
      String xpriority = xmime.getHeader("Importance", null);

      if (xpriority != null) {
        xpriority = xpriority.toLowerCase();
        if (xpriority.indexOf("high") == 0) {
          return true;
        }
      }
      // X Standard: X-Priority: 1 | 2 | 3 | 4 | 5 (lowest)
      xpriority = xmime.getHeader("X-Priority", null);

      if (xpriority != null) {
        xpriority = xpriority.toLowerCase();
        if (xpriority.indexOf("1") == 0 ||
                xpriority.indexOf("2") == 0) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Determin if the message is low-priority.
   *
   * @param message the message to examine
   * @return true if the message is low-priority
   */
  public static boolean isLowPriority(javax.mail.Message message) throws MessagingException {

    if (message instanceof MimeMessage) {
      MimeMessage xmime = (MimeMessage) message;
      String xpriority = xmime.getHeader("Importance", null);

      if (xpriority != null) {
        xpriority = xpriority.toLowerCase();
        if (xpriority.indexOf("low") == 0) {
          return true;
        }
      }
      // X Standard: X-Priority: 1 | 2 | 3 | 4 | 5 (lowest)
      xpriority = xmime.getHeader("X-Priority", null);

      if (xpriority != null) {
        xpriority = xpriority.toLowerCase();
        if (xpriority.indexOf("4") == 0 ||
                xpriority.indexOf("5") == 0) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * A 'safe' version of JavaMail getFileName() which doesn't throw exceptions.
   * Encoded filenames are also decoded if necessary.
   * Why doesn't JAVA Mail do this?
   *
   * @param part The part to interogate
   * @return File name of the part, or null if missing or invalid
   * @see javax.mail.Part
   */
  public static String getFileName(Part part) {

    String xname = null;
    try {
      xname = part.getFileName();
    } catch (MessagingException xex) {
    }
    // decode the file name if necessary
    if (xname != null && xname.startsWith("=?")) {
      try {
        xname = MimeUtility.decodeWord(xname);
      } catch (Exception xex) {
      }
    }
    return xname;
  }

  /**
   * A better version of setFileName() which will encode the name if necessary.
   * Why doesn't JAVA Mail do this?
   *
   * @param part    the part to manipulate
   * @param name    the give file name encoded in UTF (JAVA)
   * @param charset the encoding character set
   */
  public static void setFileName(Part part, String name, String charset) throws MessagingException {

    try {
      name = MimeUtility.encodeWord(name, charset, null);
    } catch (Exception xex) {
    }
    part.setFileName(name);
  }

  public static String getPrintableMD5(StringBuffer xmlSync) {
    MessageDigest md = null;
    try {
      md = MessageDigest.getInstance("MD5");
    } catch (NoSuchAlgorithmException e) {
      Tracer.emailLogger.error(e);
    }

    md.reset();
    md.update(xmlSync.toString().getBytes());
    byte md5b[] = md.digest();

    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < md5b.length; ++i)
      sb.append(Integer.toString((md5b[i] & 0xff) + 0x100, 16).substring(1));
    String md5 = sb.toString();
    return md5;
  }

}



