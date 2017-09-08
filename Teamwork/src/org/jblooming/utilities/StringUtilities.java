package org.jblooming.utilities;

import net.sf.json.JSONObject;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.waf.settings.I18n;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import javax.xml.bind.DatatypeConverter;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @author Pietro Polsinelli dev@open-lab.com
 */
public class StringUtilities {


  //128 bit key
  public static String key = "c9ae9b98d3d6b3c7429c4849afad25eb";


  /**
   * Split the given String into tokens.
   * <p/>
   * This method is meant to be similar to the split
   * function in other programming languages but it does
   * not use regular expressions.  Rather the String is
   * split on a single String literal.
   * <p/>
   * Unlike java.utilities.StringTokenizer which accepts
   * multiple character tokens as delimiters, the delimiter
   * here is a single String literal.
   * <p/>
   * Each null token is returned as an empty String.
   * Delimiters are never returned as tokens.
   * <p/>
   * If there is no delimiter because it is either empty or
   * null, the only element in the result is the original String.
   * <p/>
   * StringHelper.split("1-2-3", "-");<br>
   * result: {"1", "2", "3"}<br>
   * StringHelper.split("-1--2-", "-");<br>
   * result: {"", "1", ,"", "2", ""}<br>
   * StringHelper.split("123", "");<br>
   * result: {"123"}<br>
   * StringHelper.split("1-2---3----4", "--");<br>
   * result: {"1-2", "-3", "", "4"}<br>
   *
   * @param s         String to be split.
   * @param delimiter String literal on which to split.
   * @return an array of tokens.
   * @throws NullPointerException if s is null.
   */

  public static String[] splitToArray(String s, String delimiter) {

    int delimiterLength;
    // the next statement has the side effect of throwing a null pointer
    // exception if s is null.
    int stringLength = s.length();

    if (delimiter == null || (delimiterLength = delimiter.length()) == 0) {
      // it is not inherently clear what to do if there is no delimiter
      // On one hand it would fill sense to return each character because
      // the null String can be found between each pair of characters in
      // a String.  However, it can be found many times there and we don'
      // want to be returning multiple null tokens.
      // returning the whole String will be defined as the correct behavior
      // in this instance.
      return new String[]{s};
    }

    int count = count(s, delimiter, delimiterLength);

    int start;
    int end;

    // allocate an array to return the tokens,
    // we now know how big it should be
    String[] result = new String[count];

    // Scan s again, but this time pick out the tokens
    count = 0;
    start = 0;
    while ((end = s.indexOf(delimiter, start)) != -1) {
      result[count] = (s.substring(start, end));
      count++;
      start = end + delimiterLength;
    }
    end = stringLength;
    result[count] = s.substring(start, end);

    return (result);
  }

  public static LinkedHashSet<String> splitToOrderSet(String s, String delimiter) {
    return new LinkedHashSet(splitToList(s, delimiter));
  }

  public static Set<String> splitToSet(String s, String delimiter) {
    return new HashSet(splitToList(s, delimiter));
  }

  public static List<String> splitToList(String s, String delimiter) {

    int delimiterLength;
    // the next statement has the side effect of throwing a null pointer
    // exception if s is null.
    int stringLength = s.length();

    if (delimiter == null || (delimiterLength = delimiter.length()) == 0) {
      // it is not inherently clear what to do if there is no delimiter
      // On one hand it would fill sense to return each character because
      // the null String can be found between each pair of characters in
      // a String.  However, it can be found many times there and we don'
      // want to be returning multiple null tokens.
      // returning the whole String will be defined as the correct behavior
      // in this instance.
      List one = new ArrayList();
      one.add(s);
      return one;
    }

    int start;
    int end;

    // allocate an array to return the tokens,
    // we now know how big it should be
    List result = new ArrayList();

    if (stringLength==0)
      return result;

    // Scan s again, but this time pick out the tokens
    start = 0;
    while ((end = s.indexOf(delimiter, start)) != -1) {
      result.add(s.substring(start, end).trim());
      start = end + delimiterLength;
    }
    end = stringLength;
    result.add(s.substring(start, end).trim());

    return result;
  }

  public static Map<String, String> splitToMap(String s, String pairListDelimiter, String pairDelimiter) {
    List<String> l = splitToList(s, pairListDelimiter);
    Map<String, String> m = new HashTable();
    for (String k : l) {
      String[] strings = k.split(pairDelimiter);
      m.put(strings[0], strings[1]);
    }
    return m;
  }

  public static int count(String s, String delimiter, int delimiterLength) {
    // a two pass solution is used because a one pass solution would
    // require the possible resizing and copying of memory structures
    // In the worst case it would have to be resized n times with each
    // resize having a O(n) copy leading to an O(n^2) algorithm.

    int count;
    int start;
    int end;

    // Scan s and count the tokens.
    count = 0;
    start = 0;
    while ((end = s.indexOf(delimiter, start)) != -1) {
      count++;
      start = end + delimiterLength;
    }
    count++;
    return count;
  }

  public static int count(String s, char separator) {
    int count = 0;
    for (int i = 0; i < s.length(); i++) {
      if (s.charAt(i) == separator)
        count++;
    }
    return count;
  }


  public static String unSplit(Collection<String> coll, String delimiter) {
    StringBuffer sb = new StringBuffer(512);
    if (coll != null && coll.size() > 0) {
      boolean first = true;
      for (String s : coll) {
        if (!first)
          sb.append(delimiter);
        sb.append(s);
        first = false;
      }
    }
    return sb.toString();
  }


  public static String toMixedCase(String source, String separator) {
    source = source.trim();

    String result = "";
    for (int i = 0; i < source.length(); ) {

      i = source.indexOf(separator, i + 1);
      if (i > -1) {

        String articleCandidate = source.substring(result.length(), i);

        if (articleCandidate.toLowerCase().equals("delle") || articleCandidate.toLowerCase().equals("di")
                || articleCandidate.toLowerCase().equals("del") || articleCandidate.toLowerCase().equals("e")
                || articleCandidate.toLowerCase().equals("ed"))
          articleCandidate = source.substring(result.length(), i + 1).toLowerCase();
        else
          articleCandidate = source.substring(result.length(), result.length() + 1).toUpperCase() + source.substring(result.length() + 1, i + 1).toLowerCase();

        result = result + articleCandidate;

      } else
        result =
                result +
                        source.substring(source.lastIndexOf(" ") + 1, source.lastIndexOf(" ") + 2).toUpperCase() +
                        source.substring(source.lastIndexOf(" ") + 2).toLowerCase();

      if (i < 1)
        break;
    }

    return result;
  }

  public static String stripToLegal(String source) {
    return JSP.ex(source) ? source.replaceAll("[^.a-zA-Z0-9\\s]", "") : "";
  }

  public static String dot(String source) {

    String result = "";
    if (source != null)
      for (int i = 0; i < (source.length() - 1); ) {

        result = result + source.substring(i, i + 1) + '.';
        i++;
      }
    return result + source.substring(source.length() - 1, source.length());
  }

  /**
   * @param pattern
   * @param input
   * @return the  matches of <code>input</code> sequence against a pattern <code>pattern</code> cleaned from '*'
   */
  public static int matches(String pattern, StringBuffer input) {
    int position = 0;
    int count = 0;
    pattern = pattern.replaceAll("\\*", "");
    while (input.indexOf(pattern, position) >= position) {
      count++;
      position = input.indexOf(pattern, position) + 1;
    }
    return count;
  }

  public static int matches(Collection patterns, StringBuffer input) {
    int matchCount = 0;
    for (Iterator it = patterns.iterator(); it.hasNext(); )
      matchCount += matches((String) it.next(), input);
    return matchCount;
  }

  /**
   * Replace * and ? with corresponding regular expression. example: friendlyMatch("p*o?o", "pippolo") return true
   *
   * @param pattern
   * @param input
   * @return true if found
   */
  public static boolean friendlyMatch(String pattern, String input) {
    return input.matches(replaceAllNoRegex(replaceAllNoRegex(pattern, "*", ".*"), "?", ".?"));
  }

  public static final String md5Encode(String toBeEncoded) {
    return md5Encode(toBeEncoded, "COM.TWPROJECT");
  }

  public static final String md5Encode(String toBeEncoded, String saltAndPepper) {
    try {
      return getHash(saltAndPepper + toBeEncoded, "MD5");
    } catch (NoSuchAlgorithmException e) {
      throw new PlatformRuntimeException(e);
    }

  }

  public static final String shaEncode(String toBeEncoded) {
    return shaEncode(toBeEncoded, "COM.TWPROJECT");
  }

  public static final String shaEncode(String toBeEncoded, String saltAndPepper){
    try {
      return getHash(saltAndPepper+toBeEncoded,"SHA-256");
    } catch (NoSuchAlgorithmException e) {
      throw new PlatformRuntimeException(e);
    }
  }


  public static String getHash(String toBeEncoded, String encodeAlgorithm) throws NoSuchAlgorithmException {
    if (toBeEncoded != null) { //&& toBeEncoded.trim().length()>0) {

      MessageDigest digest = MessageDigest.getInstance(encodeAlgorithm);
      digest.update(toBeEncoded.getBytes());
      byte bytes[] = digest.digest();
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < bytes.length; i++) {
        int b = bytes[i] & 0xff;
        if (b < 16) {
          buffer.append('0');
        }
        buffer.append(Integer.toHexString(b));
      }

      toBeEncoded = buffer.toString();
    }
    return toBeEncoded;
  }

  public static final String replaceAllNoRegex(String source, String search, String replace) {
    StringBuffer buffer = new StringBuffer();
    if (JSP.ex(source)) {
      if (search.length() == 0)
        return source;
      int oldPos, pos;
      for (oldPos = 0, pos = source.indexOf(search, oldPos); pos != -1; oldPos = pos + search.length(), pos = source.indexOf(search, oldPos)) {
        buffer.append(source.substring(oldPos, pos));
        buffer.append(replace);
      }
      if (oldPos < source.length())
        buffer.append(source.substring(oldPos));
    }
    return new String(buffer);
  }

  public static final int occurrences(String source, String search) {
    int result = 0;
    int pos = 0;
    while ((pos = source.indexOf(search, pos)) > -1) {
      result++;
    }
    return result;
  }

  public static final String replaceAllNoRegex(String source, String searches[], String replaces[]) {
    int k;
    String tmp = source;
    for (k = 0; k < searches.length; k++)
      tmp = StringUtilities.replaceAllNoRegex(tmp, searches[k], replaces[k]);
    return tmp;
  }

  public static final int getMaxMemberLength(String[] array) {
    if (array != null && array.length > 0) {
      int maxLength = 0;
      for (int k = 0; k < array.length; k++)
        if (array[k].length() > maxLength)
          maxLength = array[k].length();
      return maxLength;
    } else
      return 0;
  }

  public static final String getRepeated(String seed, int count) {
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < count; i++) {
      sb.append(seed);
    }
    return sb.toString();
  }

  public static String paddTo(Object number, String padder) {
    String result = padder;
    StringBuffer combined = new StringBuffer(padder);
    combined.append(JSP.w(number));
    result = combined.substring(combined.length() - padder.length());
    return result;
  }


  public static String paddTo(int number, String padder) {
    return paddTo(number + "", padder);
  }

  public static String generatePassword(int length) {
    String[] alpha = {"A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"};
    int[] numerics = {1, 2, 3, 4, 5, 6, 7, 8, 9};
    int numberOfAlpha = 0;
    int numberOfNumeric = 0;
    StringBuffer password = new StringBuffer();

    Random r = new Random();

    for (int i = 0; i < length; i++) {
      // randomly create the alpha or numeric
      if (r.nextBoolean()) { // create alpha
        password.append(alpha[r.nextInt(alpha.length)]);
        numberOfAlpha++;
      } else {
        password.append(numerics[r.nextInt(numerics.length)]);
        numberOfNumeric++;
      }
    }

    return password.toString();
  }

  public static String arrayToString(String[] values, String separator) {
    String value = "";
    if (values == null)
      return value;
    for (int i = 0; i < values.length; i++) {
      value += values[i];
      if (i < (values.length - 1))
        value += separator;
    }
    return value;
  }

  public static String setToString(Collection<String> values, String separator) {
    String value = "";
    if (values == null)
      return value;
    int i = 0;
    for (String val : values) {
      value += val;
      if (i < (values.size() - 1))
        value += separator;
      i++;
    }
    return value;
  }

  /**
   * @param toParse
   * @return -1 if it ain't numeric, the parsed int otherwise
   */
  public static int isPositiveNumeric(String toParse) {
    int i = -1;
    try {
      i = Integer.parseInt(toParse);
    } catch (NumberFormatException e) {
    }
    return i;

  }

  public static String deCamel(String cammelled) {
    boolean justFoundACapital = false;
    boolean nextOneIsCapital = false;
    StringBuffer result = new StringBuffer();
    for (int i = 0; i < cammelled.length(); i++) {

      char myChar = cammelled.charAt(i);
      nextOneIsCapital = ((i + 1) < cammelled.length()) && Character.isLetter(cammelled.charAt(i + 1)) && !Character.isLowerCase(cammelled.charAt(i + 1));

      //detach word if moving from upper to lower

      if (
              Character.isLetter(myChar) &&
                      !Character.isLowerCase(myChar) &&
                      !nextOneIsCapital &&
                      (i + 1) < cammelled.length()

              )
        result.append(" ");

      if (
              Character.isLetter(myChar) &&
                      !Character.isLowerCase(myChar) &&
                      !justFoundACapital

              ) {
        //do not lower first char
        if (i > 0) {
          result.append(" ");
          if (!nextOneIsCapital)
            result.append((myChar + "").toLowerCase());
          else
            result.append(myChar + "");
        } else
          result.append(myChar);
        justFoundACapital = true;
      } else {
        if (!nextOneIsCapital && (i + 1) < cammelled.length())
          result.append((myChar + "").toLowerCase());
        else
          result.append(myChar);
      }
      if (Character.isLowerCase(myChar))
        justFoundACapital = false;
    }
    return result.toString().trim();
  }

  public static int countConsecutiveOccurrences(String pattern, String target) {
    int result = 0;
    int pl = pattern.length();
    for (int i = 0; i < target.length(); i = i + pl) {
      if (!target.substring(i, i + pl).equals(pattern))
        break;
      result++;
    }
    return result;
  }

  public static String normalize(String label) {
    if (label != null) {
      //return label.toLowerCase().replace(' ', '_').replace('-', '_').replace('/', '_').replace('\\', '_').replace('"', '_').replace(',', '_').replace(';', '_').replace("'","_");
      return label.replace(' ', '_').replace('-', '_').replace('/', '_').replace('\\', '_').replace('"', '_').replace(',', '_').replace(';', '_').replace("'", "_");
    } else
      return null;
  }


  public static String convert(String input, String decodeType, String encodeType) {
    CharsetDecoder decoder = Charset.forName(decodeType).newDecoder();
    CharsetEncoder encoder = Charset.forName(encodeType).newEncoder();
    String s = "";
    try {
      // Convert a string to encodeType bytes in a ByteBuffer
      // The new ByteBuffer is ready to be read.
      ByteBuffer bbuf = encoder.encode(decoder.decode(ByteBuffer.wrap(input.getBytes())));

      // Convert encodeType bytes in a ByteBuffer to a character ByteBuffer and then to a string.
      // The new ByteBuffer is ready to be read.
      CharBuffer cbuf = decoder.decode(bbuf);
      s = cbuf.toString();
    } catch (CharacterCodingException e) {
      e.printStackTrace();
    }
    return s;
  }

  public static String convert(String input) {
    CharsetDecoder decoder = Charset.forName("UTF-16").newDecoder();
    CharsetEncoder encoder = Charset.forName("ISO-8859-1").newEncoder();

    String s = "";
    try {
      // Convert a string to ISO-LATIN-1 bytes in a ByteBuffer
      // The new ByteBuffer is ready to be read.
      ByteBuffer bbuf = encoder.encode(decoder.decode(ByteBuffer.wrap(input.getBytes())));

      // Convert ISO-LATIN-1 bytes in a ByteBuffer to a character ByteBuffer and then to a string.
      // The new ByteBuffer is ready to be read.
      CharBuffer cbuf = decoder.decode(bbuf);
      s = cbuf.toString();
    } catch (CharacterCodingException e) {
      e.printStackTrace();
    }
    return s;
  }

  public static String convertHtmlToTxt(String s) {
    String[] newLines = {"<br>", "<p>", "</p>", "<table>", "</table>", "<td>", "</td>", "<tr>", "</tr>", "<a>", "</a>"};

    if (s != null) {
      int startTag = 0;
      while ((startTag = s.indexOf("<", startTag)) != -1) {
        String breaker = "";
        int endTag = s.indexOf(">", startTag);
        if (endTag != -1) {
          String tag = s.substring(startTag, endTag + 1);
          for (String newLine : newLines)
            if (newLine.equals(tag))
              breaker = "\n";
          s = s.substring(0, startTag) + breaker + s.substring(endTag + 1, s.length());
        } else
          startTag++;
      }
    }

    return s;
  }

  public static StringBuffer replace(StringBuffer buf, int start, int end, String text) {
    int len = text.length();
    char[] ch = new char[buf.length() + len - (end - start)];
    buf.getChars(0, start, ch, 0);
    text.getChars(0, len, ch, start);
    buf.getChars(end, buf.length(), ch, start + len);
    buf.setLength(0);
    buf.append(ch);
    return buf;
  }

  /**
   * @param target     is a string containing "%%" like "my name is Gino and I like %% very much, and %% too"
   * @param parameters "apple" "linux"
   * @return
   */
  public static String replaceParameters(String target, String... parameters) {
    if (parameters != null) {
      for (String t : parameters) {
        target = target.replaceFirst("%%", Matcher.quoteReplacement(t != null ? t : "-"));
      }
    }
    return target;
  }

  public static String string2Hex(String s) {
    String rs2 = "";
    for (int i = 0; i < s.length(); ++i) {
      String hexValue = Integer.toHexString(s.charAt(i));
      rs2 += (hexValue.length() == 1 ? "0" + hexValue : hexValue);
    }
    return rs2.toUpperCase();
  }

  public static List<CodeValue> splitToCodeValueList(String clob, String listSeparator, String codevalueSeparator) {
    List<String> s = splitToList(clob, listSeparator);
    List<CodeValue> cvl = new ArrayList();
    for (String s1 : s) {
      List<String> s2 = splitToList(s1, codevalueSeparator);
      CodeValue cv = new CodeValue(s2.get(0), s2.get(1));
      cvl.add(cv);
    }
    return cvl;
  }


  public static String asHex(byte buf[]) {
    StringBuffer strbuf = new StringBuffer(buf.length * 2);
    int i;
    for (i = 0; i < buf.length; i++) {
      if (((int) buf[i] & 0xff) < 0x10)
        strbuf.append("0");
      strbuf.append(Long.toString((int) buf[i] & 0xff, 16));
    }
    return strbuf.toString();
  }


  public static byte[] stringToByteArray(String bstr) {
    if (bstr == null) return null;
    int sz = bstr.length();
    byte[] bytes = new byte[sz / 2];
    for (int i = 0; i < sz / 2; i++) {
      bytes[i] = stringToByte(bstr.substring(2 * i, 2 * i + 2));
    }
    return bytes;
  }

  private static byte stringToByte(String s) {
    try {
      return (byte) Integer.parseInt(s, 16);
    } catch (NumberFormatException nfe) {
      return 0;
    }
  }

  public static String generateAESKey() {
    String k = null;
    try {
      KeyGenerator kgen = KeyGenerator.getInstance("AES");
      kgen.init(128);
      SecretKey skey = kgen.generateKey();
      k = asHex(skey.getEncoded());
    } catch (Throwable t) {
    }
    return k;
  }

  public static String encrypt(String textToEncrypt) {
    return encrypt(textToEncrypt, I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key );
  }

  public static String encrypt(String textToEncrypt, String passedKey) {
    if (!JSP.ex(textToEncrypt))
      return "";
    byte[] rawKey = stringToByteArray(passedKey);
    try {
      SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
      Cipher cipher = Cipher.getInstance("AES");
      cipher.init(Cipher.ENCRYPT_MODE, skeySpec);
      byte[] encrypted = cipher.doFinal((textToEncrypt.getBytes()));
      return (asHex(encrypted));
    } catch (Exception ex) {
      throw new PlatformRuntimeException(ex);
    }
  }

  public static String encryptBase64(String textToEncrypt, String passedKey) {
    if (!JSP.ex(textToEncrypt))
      return "";
    byte[] rawKey = stringToByteArray(passedKey);
    try {
      SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
      Cipher cipher = Cipher.getInstance("AES");
      cipher.init(Cipher.ENCRYPT_MODE, skeySpec);
      byte[] encrypted = cipher.doFinal((textToEncrypt.getBytes()));
      return (DatatypeConverter.printBase64Binary(encrypted));
    } catch (Exception ex) {
      throw new PlatformRuntimeException(ex);
    }
  }


  public static String decrypt(String textToDecrypt) {
    return decrypt(textToDecrypt, I18n.isActive("CUSTOM_FEATURE_AES_CRYPTO_KEY") ? I18n.get("CUSTOM_FEATURE_AES_CRYPTO_KEY") : StringUtilities.key );
  }

  public static String decrypt(String textToDecrypt, String passedKey) {
    byte[] rawKey = stringToByteArray(passedKey);
    try {
      Cipher cipher = Cipher.getInstance("AES");
      SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
      cipher.init(Cipher.DECRYPT_MODE, skeySpec);
      byte[] original = cipher.doFinal(stringToByteArray(textToDecrypt));
      return new String(original);
    } catch (Exception ex) {
      throw new PlatformRuntimeException(ex);
    }
  }

  public static String decryptBase64(String textToDecrypt, String passedKey) {
    byte[] rawKey = stringToByteArray(passedKey);
    try {
      Cipher cipher = Cipher.getInstance("AES");
      SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
      cipher.init(Cipher.DECRYPT_MODE, skeySpec);
      byte[] original = cipher.doFinal(DatatypeConverter.parseBase64Binary(textToDecrypt.replaceAll("--.*--", "").replaceAll("\\s", "")));
      return new String(original);
    } catch (Exception ex) {
      throw new PlatformRuntimeException(ex);
    }
  }


  /*this method return the url contained in a string, if more then one is found it returns only the first*/
  public static String findUrlInString(String string) {
    String url = "";
    if (JSP.ex(string)) {

      Pattern pattern = Pattern.compile("(['\"]\\s*)?(http[s]?:[\\d]*\\/\\/[^\"<>\\s]*)");
      Matcher matcher = pattern.matcher(string);
      boolean urlFound = false;
      boolean matchFound = matcher.find();

      if (matchFound) {
        for (int i = 0; i <= matcher.groupCount(); i++) {
          String groupStr = matcher.group(i);
          if (JSP.ex(groupStr) && groupStr.toLowerCase().startsWith("http")) {
            url = groupStr;
            urlFound = true;
          }
        }
      }
      pattern = Pattern.compile("(['\"/]\\s*)?(www.[^\"<>\\s]*)");
      matchFound = matcher.find();

      if (matchFound && !urlFound) {
        for (int i = 0; i <= matcher.groupCount(); i++) {
          String groupStr = matcher.group(i);
          if (JSP.ex(groupStr) && groupStr.toLowerCase().startsWith("www")) {
            url = groupStr;
            urlFound = true;
          }
        }
      }
    }
    return url;
  }

  public static String capitalize(String s) {
    String r = s;
    if (JSP.ex(s))
      r = (s.charAt(0) + "").toUpperCase() + (s.length() > 1 ? s.substring(1).toLowerCase() : "");
    return r;
  }

  public static String capitalizeAll(String ss) {
    String r = ss;
    if (JSP.ex(ss)) {
      r = "";

      for (String s : splitToArray(ss, " "))
        r += capitalize(s);

      r=r.trim();
    }
    return r;
  }

  public static String generateKeyForEmail(String extRequesterEmail) {
    JSONObject jo= new JSONObject();
    jo.element("rnd",StringUtilities.generatePassword(8));
    jo.element("email",extRequesterEmail);
    jo.element("ck",StringUtilities.shaEncode(extRequesterEmail));

    return StringUtilities.encrypt(jo.toString()).toUpperCase();
  }

  /**
   * return the email if ok. null otherwise
   * @param key
   * @return
   */
  public static String generateEmailFromKey(String key) {
    String ret=null;
    try{
      JSONObject jo = JSONObject.fromObject(decrypt(key));
      if (jo.getString("ck").equals(shaEncode(jo.getString("email"))))
        ret=jo.getString("email");
    } catch (Throwable t){

    }
    return ret;
  }


  }
