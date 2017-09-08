package org.jblooming.utilities.file;

import org.jblooming.PlatformRuntimeException;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.NumberUtilities;
import org.jblooming.utilities.JSP;

import javax.crypto.NoSuchPaddingException;
import javax.servlet.http.HttpServletRequest;
import javax.crypto.spec.SecretKeySpec;
import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.CipherInputStream;
import java.io.*;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.channels.FileChannel;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

/**
 * Work with files in file system (delete, get size, etc...).
 *
 * @author Roberto Bicchierai & Pietro Polsinelli
 * @since JDK 1.4
 */
public class FileUtilities {

  public enum ImageExtensions {
    jpg, jpeg, gif, bmp, png, dwg, svg
  }

  public enum DocExtensions {
    //doc, docx, pdf, txt, rtf
    pdf, rtf, txt,doc,docx,dotx,docEmb,msg,msgEmb
  }

  public enum PdfExtensions {
    //doc, docx, pdf, txt, rtf
    pdf, ps
  }

  public enum FlashExtension {
    swf, flv
  }

  public enum QuickExtension {
    mov, mp4
  }

  public enum WindowsMovieExtension {
    wmv, avi, mpeg, mpg
  }

  public enum AppletExtension {
    jar
  }

  public enum JspExtension {
    jsp
  }

  public enum AudioExtension {
    mp3, m4a, wav, wma
  }

  public enum ArchiveExtension {
    zip, rar
  }

  public enum MsApplicationExtension {
    xls,xlsx,xltx,xlsEmb,ppt,pptx,pps,vsd,pub,mdb,mpx,mpp
  }

  public enum ExecutableExtension {
    exe, bin, com, bat, sh, dmg, rpm, msi
  }

  public static boolean isImageByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < ImageExtensions.values().length; i++) {
      ImageExtensions imageExtensions = ImageExtensions.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+imageExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isPdfByFileExt(String fileExt) {
    boolean result = false;
    for (PdfExtensions docExt : PdfExtensions.values()) {
      if (fileExt != null && fileExt.toLowerCase().equals("."+docExt.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }
  public static boolean isDocByFileExt(String fileExt) {
    boolean result = false;
    for (DocExtensions docExt : DocExtensions.values()) {
      if (fileExt != null && fileExt.toLowerCase().equals("."+docExt.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isQuickByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < QuickExtension.values().length; i++) {
      QuickExtension quickExtensions = QuickExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+quickExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isWindowsMovieByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < WindowsMovieExtension.values().length; i++) {
      WindowsMovieExtension movieExtensions = WindowsMovieExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+movieExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static void encrypt(File in, File out, String key)  {
    try {
    encrypt(new FileInputStream(in),new FileOutputStream(out),key );
    } catch (Throwable ex) {
      throw new PlatformRuntimeException(ex);
    }
  }

  public static void encrypt(InputStream in, OutputStream out, String key) {
    try {
      CipherOutputStream os = getCipherOutputStream(out, key);
      copy(in, os);
    } catch (Throwable ex) {
      throw new PlatformRuntimeException(ex);
    }
  }

  public static CipherOutputStream getCipherOutputStream(OutputStream out, String key) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException {
    byte[] rawKey = StringUtilities.stringToByteArray(key);
    SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
    Cipher cipher = Cipher.getInstance("AES");
    cipher.init(Cipher.ENCRYPT_MODE, skeySpec);
    return new CipherOutputStream(out, cipher);
  }


  public static void decrypt(File in, File out, String key) {
    try {
      decrypt(new FileInputStream(in),new FileOutputStream(out),key );
    } catch (Throwable ex) {
      throw new PlatformRuntimeException(ex);
    }
  }
  public static void decrypt(InputStream in, OutputStream out, String key) {
    try {
      CipherInputStream is = getCipherInputStream(in, key);
      copy(is, out);

    } catch (Exception ex) {
      throw new PlatformRuntimeException(ex);
    }

  }

  public static CipherInputStream getCipherInputStream(InputStream in, String key) throws NoSuchAlgorithmException, NoSuchPaddingException, InvalidKeyException {
    byte[] rawKey = StringUtilities.stringToByteArray(key);
    Cipher cipher = Cipher.getInstance("AES");
    SecretKeySpec skeySpec = new SecretKeySpec(rawKey, "AES");
    cipher.init(Cipher.DECRYPT_MODE, skeySpec);

    return new CipherInputStream(in, cipher);
  }


  public static boolean isFlashByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < FlashExtension.values().length; i++) {
      FlashExtension flashExtensions = FlashExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+flashExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isJspByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < JspExtension.values().length; i++) {
      JspExtension jspExt = JspExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+jspExt.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isAppletByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < AppletExtension.values().length; i++) {
      AppletExtension appletExtensions = AppletExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+appletExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isAudiotByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < AudioExtension.values().length; i++) {
      AudioExtension audioExtensions = AudioExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+audioExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isArchiveByFileExt(String fileExt) {
    boolean result = false;
    for (int i = 0; i < ArchiveExtension.values().length; i++) {
      ArchiveExtension archiveExtensions = ArchiveExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+archiveExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isMsApplicationExtension(String fileExt) {
    boolean result = false;
    for (int i = 0; i < MsApplicationExtension.values().length; i++) {
      MsApplicationExtension msExtensions = MsApplicationExtension.values()[i];
      if (fileExt != null && fileExt.toLowerCase().equals("."+msExtensions.toString().toLowerCase())) {
        result = true;
        break;
      }
    }
    return result;
  }

  public static boolean isExecutableExtension(String fileExt) {
      boolean result = false;
      for (int i = 0; i < MsApplicationExtension.values().length; i++) {
        ExecutableExtension executableExtension = ExecutableExtension.values()[i];
        if (fileExt != null && fileExt.toLowerCase().equals("."+executableExtension.toString().toLowerCase())) {
          result = true;
          break;
        }
      }
      return result;
    }


  /**
   * Copy source to dest and close both streams
   * @param source
   * @param dest
   * @throws IOException
   */
  public static void copy(InputStream source, OutputStream dest) throws IOException {
    copy(source, dest, true);
  }

  public static void copy(InputStream source, OutputStream dest, boolean closeStreams) throws IOException {
    byte[] bucketBuffer= new byte[4096];
    for (; ;) {
      int read = source.read(bucketBuffer);
      if (read < 0)
        break;
      dest.write(bucketBuffer, 0, read);
    }
    dest.flush();
    if (closeStreams) {
      safeClose(source, dest);
    }
  }

  private static void safeClose(InputStream source, OutputStream dest) throws IOException {
    try {
      source.close();
    } finally {
      dest.close();
    }
  }

  /**
   * Not highly optimized (bytes are copyed at least twice in memory)
   */
  public static byte[] getStreamBytes(InputStream source) throws IOException {
    return getStreamBytes(source, Math.max(source.available(), 1024));
  }

  /**
   * Not highly optimized (bytes are copyed at least twice in memory)
   */
  static byte[] getStreamBytes(InputStream source, long estimatedSize) throws IOException {
    if (source instanceof FileInputStream)
      return getFileInputStreamBytes(estimatedSize, (FileInputStream) source);
    else
      return getGenericStreamBytes(estimatedSize, source);
  }

  /**
   * Should not be used with files > 2 gibabytes in size
   *
   * @param estimatedSize
   * @param source
   * @return a byte array containing bytes stored in the underlying file
   * @throws IOException
   */
  private static byte[] getFileInputStreamBytes(long estimatedSize, FileInputStream source) throws IOException {
    // far more efficent but 1.4 specific  : PENDING not yet tested
    int size = (int) source.getChannel().size();  // requires java.nio ; may lead to problems with files with size > Integer.MAX_VALUE (2 giga)
    int pos = (int) source.getChannel().position();  // requires java.nio ; may lead to problems with files with size > Integer.MAX_VALUE (2 giga)
    size -= pos;
    byte[] buffer = new byte[size];
    int ofs = 0;
    while (size < ofs) {
      int read = source.read(buffer, ofs, size - ofs);
      if (read < 0)
        break;
      ofs += read;
    }
    if (ofs != size)
      throw new IOException("Read " + ofs + " bytes instead of " + size);
    return buffer;
  }

  static byte[] getGenericStreamBytes(long estimatedSize, InputStream source) throws IOException {
    ByteArrayOutputStream dest = new ByteArrayOutputStream((int) Math.min(Integer.MAX_VALUE, estimatedSize));
    copy(source, dest);
    return dest.toByteArray();
  }

  public static void copyBytes(String sourcePath, String destPath) throws IOException {
    InputStream source = new FileInputStream(sourcePath);
    OutputStream dest = new FileOutputStream(destPath);
    copy(source, dest);
  }

  public static void copyBytes(File sourceFile, File destFile) throws IOException {
    InputStream source = new FileInputStream(sourceFile);
    OutputStream dest = new FileOutputStream(destFile);
    copy(source, dest);
  }

  ////////////////////////////// Text file processing ////////////////////////////////////////////

  /**
   * Closes reader and writer
   *
   * @param source
   * @param dest
   * @throws IOException
   */
  public static void copy(Reader source, Writer dest) throws IOException {
    copy(source, dest, true);
  }

  public static void copy(Reader source, Writer dest, boolean closeStreams) throws IOException {
    copy(source, dest, 4096, closeStreams);
  }

  public static void copy(Reader source, Writer dest, int bufferSize, boolean closeStreams) throws IOException {
    char[] buffer = new char[bufferSize];
    copy(source, dest, buffer, closeStreams);
  }

  /**
   * Paranoid variant : available to avoid bucket buffer reallocation in multiple
   * copy but internally used by all other variants of copy
   *
   * @see #copy
   */
  public static void copy(Reader source, Writer dest, char[] bucketBuffer, boolean closeStreams) throws IOException {
    for (; ;) {
      int read = source.read(bucketBuffer);
      if (read < 0)
        break;
      dest.write(bucketBuffer, 0, read);
    }
    dest.flush();
    if (closeStreams) {
      safeClose(source, dest);
    }
  }

  private static void safeClose(Reader source, Writer dest) throws IOException {
    try {
      source.close();
    } finally {
      dest.close();
    }
  }

  /**
   * puts into a string all chard read from a Reader
   * - author Nicola Ponzeveroni
   *
   * @param source
   * @param estimatedSize used for optimization - is the size of the 'pre-allocated' internally used buffer
   * @throws IOException
   */
  public static String getReaderContent(Reader source, long estimatedSize) throws IOException {
    StringWriter dest = new StringWriter((int) Math.min(Integer.MAX_VALUE, estimatedSize));
    copy(source, dest);
    StringBuffer sb = dest.getBuffer();
    return sb.toString();
  }

  /**
   * Copy using a reader with default encoding
   *
   * @param sourcePath
   * @param destPath
   * @throws IOException author Nicola Ponzeveroni
   */
  public static void copyChars(String sourcePath, String destPath) throws IOException {
    Reader source = new FileReader(sourcePath);
    Writer dest = new FileWriter(destPath);
    copy(source, dest);
  }

  /**
   * Copy using a reader with default encoding
   * - author Nicola Ponzeveroni
   *
   * @param sourceFile
   * @param destFile
   * @throws IOException
   */
  public static void copyChars(File sourceFile, File destFile) throws IOException {
    Reader source = new FileReader(sourceFile);
    Writer dest = new FileWriter(destFile);
    copy(source, dest);
  }

  /**
   * Method deleteDir
   * Deletes all files and subdirectories under dir.
   * Returns true if all deletions were successful.
   * If a deletion fails, the method stops attempting to delete and returns false.
   *
   * @param dir a  File
   * @return a boolean    true if OK false if not
   */
  public static boolean deleteDir(File dir) {
    if (dir.isDirectory()) {
      String[] children = dir.list();
      for (int i = 0; i < children.length; i++) {
        boolean success = deleteDir(new File(dir, children[i]));
        if (!success) {
          return false;
        }
      }
    }
    // The directory is now empty so delete it
    return dir.delete();
  }

  public static String readTextFile(String path) throws IOException {
    StringBuffer sb = new StringBuffer();
    InputStream stream = new FileInputStream(path);
    return readInputStream(stream, sb);
  }

  public static String readInputStream(InputStream stream, StringBuffer sb) throws IOException {
    return readInputStream(stream, sb, "UTF-8");
  }

  public static String readInputStream(InputStream stream, StringBuffer sb, String charset) throws IOException {
    InputStreamReader streamReader = new InputStreamReader(stream, charset);
    BufferedReader reader = new BufferedReader(streamReader);
    while (true) {
      int cr = reader.read();
      if (cr < 0)
        break;
      sb.append((char) cr);
    }
    return sb.toString();
  }

  public static String readReader(Reader reader) throws IOException {

    StringBuffer sb = new StringBuffer();
    BufferedReader re = new BufferedReader(reader);
    String str;
    while ((str = re.readLine()) != null) {
      sb.append(str);
    }
    return sb.toString();
  }

  /**
   * @param source
   * @param dest   this was deprecated use copy; now it is suddenly highly appreciated :-D Pietro
   */
  public static void copyFile(File source, File dest) {
    //Address of the destination file as a string
    try {
      FileInputStream SourceFile = new FileInputStream(source);
      FileOutputStream DestinationFile = new FileOutputStream(dest);
      byte readFromFile[] = new byte[SourceFile.available()];

      //This is an array which contains the number of bytes that are available in the source file. So if you want to read a part of the source file (less memory usage when copying) you can change the option SourceFile.available() to any number you want.
      SourceFile.read(readFromFile);

      //This reads the number of bytes specified above from the file(i.e from byte number 0 to byte number readFromFile).Be careful with this, if you try to read a huge amount of bytes into ram you will run out of memory so be diplomatic with it, read small chunks.
      DestinationFile.write(readFromFile);

      //This writes all the things that you have read from your source file.If you want to write the file in small chunks, you can do this by putting this or part of this code into some sort of loop.
      SourceFile.close();

      //Let the file go.
      DestinationFile.close();

      //Let the file go.
    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }
  }

  public static void mycopy(File source, File dest) {
    //Address of the destination file as a string
    try {
      FileInputStream SourceFile = new FileInputStream(source);
      FileOutputStream DestinationFile = new FileOutputStream(dest);
      copy(SourceFile, DestinationFile);

    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    }
  }

  public static String getParentPath(String file) {
    int end = file.lastIndexOf(File.separator);
    if (end > 0)
      return file.substring(0, end);
    else
      return null;
  }

  public static String getNameWithoutExt(String file) {
    int end = file.lastIndexOf(".");
    if (end > 0)
      return file.substring(0, end);
    else
      return null;
  }

  public static String getFileExt(String file) {
    if (file != null && file.lastIndexOf(".") > -1) {
      return file.substring(file.lastIndexOf("."));
    } else
      return "";
  }

  public static void writeToFile(String pathAndFileName, String text, String charsetName) {

    try {
      Charset charset = Charset.forName(charsetName);
      File global = new File(pathAndFileName);

      BufferedWriter d = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(global), charset));
      d.write(text);
      d.close();

    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }

  }


  /**
   * Calls writeToFile with createDir flag false.
   */
  public static void writeToFile(String fileName, String text) throws IOException {
    copy(new StringReader(text), new FileWriter(fileName));
  }

  public static void appendToFile(String fileName, String text) throws IOException {
    copy(new StringReader(text), new FileWriter(fileName, true));
  }

  public static void writeToFile(String fileName, InputStream iStream) throws IOException {
    writeToFile(fileName, iStream, false);
  }

  /**
   * Writes InputStream to a given <code>fileName<code>.
   * And, if directory for this file does not exists,
   * if createDir is true, creates it, otherwice throws OMDIOexception.
   *
   * @param fileName  - filename save to.
   * @param iStream   - InputStream with data to read from.
   * @param createDir (false by default)
   * @throws java.io.IOException in case of any error.
   */
  public static void writeToFile(String fileName, InputStream iStream, boolean createDir) throws IOException {

    String me = "FileUtilities.WriteToFile";
    if (fileName == null) {
      throw new IOException(me + ": filename is null");
    }
    if (iStream == null) {
      throw new IOException(me + ": InputStream is null");
    }
    File theFile = new File(fileName);

    // Check if a file exists.
    if (theFile.exists()) {
      String msg =
              theFile.isDirectory() ? "directory" :
                      (!theFile.canWrite() ? "not writable" : null);
      if (msg != null) {
        throw new IOException(me + ": file '" + fileName + "' is " + msg);
      }
    }

    // Create directory for the file, if requested.
    if (createDir && theFile.getParentFile() != null) {
      theFile.getParentFile().mkdirs();
    }

    // Save InputStream to the file.
    BufferedOutputStream fOut = null;
    try {
      fOut = new BufferedOutputStream(new FileOutputStream(theFile));
      byte[] buffer = new byte[4 * 1024];
      int bytesRead = 0;
      while ((bytesRead = iStream.read(buffer)) != -1) {
        fOut.write(buffer, 0, bytesRead);
      }
    } catch (Throwable e) {
      throw new IOException(me + " failed, got: " + e.toString());
    } finally {
      close(iStream, fOut);
    }
  }

  /**
   * Closes InputStream and/or OutputStream.
   * It makes sure that both streams tried to be closed,
   * even first throws an exception.
   */
  protected static void close(InputStream iStream, OutputStream oStream) throws IOException {
    try {
      if (iStream != null) {
        iStream.close();
      }
    } finally {
      if (oStream != null) {
        oStream.close();
      }
    }
  }

  public static boolean tryHardToDeleteFile(File file) {
    return tryHardToDeleteFile(file, 10, 0);
  }

  public static boolean tryHardToDeleteFile(File file, int tries, int tried) {
    boolean didDelete = false;
    if (!file.delete()) {
      System.gc();  // In case the JVM itself thinks it has a file handle
      if (!file.delete()) {
        try {  // Now wait and see if it goes away
          Thread.sleep(200);
        } catch (InterruptedException e) {
        }
        // Potential perma-loop avoided with counter
        // if still there after specified tries, give
        // up, you've got another problem
        if (tried < tries) {
          tryHardToDeleteFile(file, tries, ++tried);
        }
      } else
        didDelete = true;
    } else
      didDelete = true;

    return didDelete;
  }

  public static void tryHardToDeleteDir(File dir) {
    if (dir.isDirectory()) {
      String[] children = dir.list();
      for (int i = 0; i < children.length; i++) {
        tryHardToDeleteDir(new File(dir, children[i]));
      }
    }
    // The directory is now empty so delete it
    tryHardToDeleteFile(dir);
  }


  public static void deleteThis(String s) {
    File d = new File(s);
    boolean result = d.delete();
  }

  public static String padd(String s, int length, String padder) {
    return NumberUtilities.padd(s, length, padder);
  }

  public static void traverseDir
          (File f,
           FileFilter filter,
           Collection fileFiltersExcluded,
           Collection directoriesExcluded,
           Visitor visitor,
           boolean isDeletion) throws Exception {

    if (f.isDirectory()) {
      boolean accepted = true;
      if (directoriesExcluded != null) {
        for (Iterator iterator = directoriesExcluded.iterator(); iterator.hasNext();) {
          String directory = (String) iterator.next();
          if (f.getAbsolutePath().endsWith(directory))
            accepted = false;
        }
      }

      if (accepted) {
        File files[] = f.listFiles(filter);
        for (int i = 0; i < files.length; i++) {
          traverseDir(files[i], filter, fileFiltersExcluded, directoriesExcluded, visitor, isDeletion);
        }
      }

    } else {

      if (refutedByAll(fileFiltersExcluded, f)) {
        if (isDeletion)
          ((FileCopy) visitor).delete(f);
        else
          visitor.visit(f);
      }
    }
  }

  public static Set<File> getFilesRecursively(File root) {
    Set<File> files = new HashSet();
    if (root.exists() && root.isDirectory()) {
      getFilesRecursivelyRecur(root, files);
    }
    return files;
  }

  private static void getFilesRecursivelyRecur(File file, Set<File> files) {
    if (file.isDirectory())
      for (File child : file.listFiles())
        getFilesRecursivelyRecur(child, files);
    else
      files.add(file);
  }

  public static Set<File> getFilesRecursively(File root, FileFilter filter) {
    Set<File> files = new HashSet();
    if (root.exists() && root.isDirectory()) {
      getFilesRecursivelyRecur(root, files, filter);
    }
    return files;
  }

  private static void getFilesRecursivelyRecur(File file, Set<File> files, FileFilter filter) {
    if (file.isDirectory())
      for (File child : file.listFiles(filter))
        getFilesRecursivelyRecur(child, files, filter);
    else
      files.add(file);
  }


  private static boolean refutedByAll(Collection filtersExcluded, File f) {
    if (filtersExcluded == null)
      return true;

    boolean result = true;

    for (Iterator iterator = filtersExcluded.iterator(); iterator.hasNext();) {
      GenericFileFilter filter = (GenericFileFilter) iterator.next();
      if (filter.accept(f))
        result = false;
    }
    return result;
  }

  public static File getTempFolder(HttpServletRequest request, String subFolderPath) throws IOException {
    String tempFolderName = request.getSession(true).getServletContext().getRealPath("/temp" + subFolderPath);
    File tempDir = new File(tempFolderName);
    if ((!tempDir.exists() && !tempDir.mkdir()) || !tempDir.isDirectory())
      throw new IOException("Can't getPageState/access temporary folder " + tempFolderName);
    return tempDir;
  }

  public static String cleanText(String s) {
    char[] ch = s.toCharArray();
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < ch.length; ++i) {

      /*
      char c = ch[i];
      if (Character.isLetterOrDigit(c)) {
      sb.append(c);
      } else {
      String value = Integer.toHexString(c);
      for (int freq = 0; freq < (2 - value.length()); ++freq)
      sb.append('0');
      if (value.length() > 2)
      value = value.substring(0, 2);
      sb.append('%').append(value);
      }
      */
    }
    return sb.toString();
  }

  /**
   * Parse a fixed length file into an array of array of Strings
   *
   * @param widths                  gives the lengths according to which we must separate the tokens
   * @param fullPathAndFile
   * @param numberOfCharForLineFeed e.g. on windows 2, on unix 1
   * @return
   * @throws IOException
   */
  public static String[][] fixedSizeParse(int[] widths, String fullPathAndFile, int numberOfCharForLineFeed) throws IOException {

    FileReader fr = new FileReader(fullPathAndFile);
    int lineLength = 0;
    for (int i = 0; i < widths.length; i++) {
      lineLength += widths[i];
    }

    int numberOfLines = (new FileInputStream(fullPathAndFile).available()) / (lineLength + numberOfCharForLineFeed);
    String[][] result = new String[numberOfLines + 1][widths.length];
    StringBuffer token = new StringBuffer();
    int currentLine = 0;
    boolean keepGoing = true;

    while (keepGoing) {
      for (int i = 0; i < widths.length; i++) {
        int width = widths[i];
        for (int j = 0; j < width; j++) {
          int k = fr.read();
          if (k < 0)
            keepGoing = false;
          if (keepGoing)
            token.append((char) k);
        }
        if (keepGoing) {
          result[currentLine][i] = token.toString();
          token = new StringBuffer();
        }
      }
      //line feeders
      for (int i = 0; i < numberOfCharForLineFeed; i++) {
        fr.read();
      }
      currentLine++;
    }
    return result;
  }

  public static String getFileNameWithExtension(String fullNameWithPath) {
    int pos = fullNameWithPath.lastIndexOf(File.separator);
    if (pos > 0)
      return fullNameWithPath.substring(pos + 1);
    else
      return fullNameWithPath;
  }

  public static String getFilePathForCurrentOS(String whateverPath) {
    return StringUtilities.replaceAllNoRegex(StringUtilities.replaceAllNoRegex(whateverPath, "\\\\", "/"), "/", File.separator);
  }

  public static Properties getProperties(String fileName) {
    Properties p = null;
    File global = new File(fileName);
    if (global.exists()) {
      p = new Properties();
      try {
        Charset charset = Charset.forName("UTF-8");
        FileInputStream stream = new FileInputStream(global);
        BufferedReader d = new BufferedReader(new InputStreamReader(stream, charset));
        String s = d.readLine();
        while (s != null) {
          int pos = s.indexOf('=');
          if (pos > 0) {
            String key = s.substring(0, pos);
            String value = s.substring(pos + 1);
            p.put(key.trim(), value.trim());
          }
          s = d.readLine();
        }
        d.close();
        stream.close();
      } catch (Throwable e) {
        throw new PlatformRuntimeException(e);
      }
    }
    return p;
  }

  public static void savePropertiesInUTF8(Properties p, String pathAndFileName) {

    File global = new File(pathAndFileName);

    try {
      Charset charset = Charset.forName("UTF-8");
      BufferedWriter d = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(global), charset));
      for (Object key : new TreeSet(p.keySet())) {
        d.write(key + "=" + p.get(key) + "\n");
      }
      d.close();
    } catch (Throwable e) {
      throw new PlatformRuntimeException(e);
    }

  }


  public static void writeStream(File file, OutputStream outputStream) {
    try {
      // Create a read-only memory-mapped file
      FileChannel roChannel = new RandomAccessFile(file, "r").getChannel();
      ByteBuffer roBuf = roChannel.map(FileChannel.MapMode.READ_ONLY, 0, (int) roChannel.size());
      while (roBuf.position() < roBuf.limit()) {
        outputStream.write(roBuf.get());
      }
      outputStream.close();

    } catch (IOException e) {
    }
  }

  public static void writeStream(InputStream inputStream, OutputStream outputStream) throws IOException {

    byte[] buffer = new byte[2048];
    while (true) {
      int amountRead = inputStream.read(buffer);
      if (amountRead == -1) {
        break;
      }
      outputStream.write(buffer, 0, amountRead);
    }
    inputStream.close();
    outputStream.close();
  }

  // Returns an output stream for a ByteBuffer.
  // The write() methods use the relative ByteBuffer put() methods.
  public static OutputStream newOutputStream(final ByteBuffer buf) {
    return new OutputStream() {
      public synchronized void write(int b) {
        buf.put((byte) b);
      }

      public synchronized void write(byte[] bytes, int off, int len) {
        buf.put(bytes, off, len);
      }
    };
  }

  public static String convertFileSize(long size) {
    return Tracer.objectSize(size);
  }

  public static long getFileSize(File folder) {
    long foldersize = 0;

    File[] filelist = folder.listFiles();
    for (int i = 0; i < filelist.length; i++) {
      if (filelist[i].isDirectory()) {
        foldersize += getFileSize(filelist[i]);
      } else foldersize += filelist[i].length();
    }

    return foldersize;
  }


}

