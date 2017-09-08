package org.jblooming.utilities;

import org.jblooming.tracer.Tracer;

import java.io.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import java.util.Set;
import java.util.HashSet;

/**
 * Date: 2-dic-2002
 * Time: 18.05.25
 *
 * @author Pietro Polsinelli mailto:pietro@twproject.com
 * @author Ezio Manetti mailto:emanetti@open-lab.com
 */
public class Zipping {


  public static void zipFile(String pathToZip, String zipPathAndName, String prefixToCut, String prefixToAdd) {

    ZipOutputStream zout = null;

    try {

      byte b[] = new byte[512];

      FileOutputStream out = new FileOutputStream(zipPathAndName);

      zout = new ZipOutputStream(out);


      File inPath = new File(pathToZip);

      if (inPath.exists() == true) {

        //if it is directory
        if (inPath.isDirectory()) {

          recursivelyZipDir(zout, inPath, prefixToCut, prefixToAdd);

          // its file
        } else {
          ZipEntry e = new ZipEntry(pathToZip);
          zout.putNextEntry(e);
        }

        if (!inPath.isDirectory()) {
          InputStream in = new FileInputStream(pathToZip);
          int len = 0;
          while ((len = in.read(b)) != -1) {
            zout.write(b, 0, len);
          }
          in.close();
        }

        zout.closeEntry();
        zout.flush();
        zout.close();

        // Close the file output streams for both the file and the zip.

      }

    } catch (IOException e) {
      Tracer.platformLogger.error(e);
    }


  }

  private static void recursivelyZipDir(ZipOutputStream zout, File inPath, String prefixToCut, String prefixToAdd) {

    File[] fileList = inPath.listFiles();

    // Loop through File array and display.
    for (int i = 0; i < fileList.length; i++) {
      if (fileList[i].isDirectory()) {
        recursivelyZipDir(zout, new File(fileList[i].getPath()), prefixToCut, prefixToAdd);
      } else if (fileList[i].isFile()) {
        // Call the zipFunc function
        zipFunc(zout, fileList[i].getPath(), prefixToCut, prefixToAdd);
      }
    }
  }


  // New zipFunc method.
  public static void zipFunc(ZipOutputStream zos, String filePath, String prefixToCut, String prefixToAdd) {
    // Using try is required because of file io.
    try {
      // Create a file input stream and a buffered input stream.
      FileInputStream fis = new FileInputStream(filePath);
      BufferedInputStream bis = new BufferedInputStream(fis);

      // Create a Zip Entry and put it into the archive (no data yet).
      ZipEntry fileEntry = new ZipEntry(prefixToAdd + filePath.substring(prefixToCut.length() + 1));

      zos.putNextEntry(fileEntry);

      // Create a byte array object named data and declare byte count variable.
      byte[] data = new byte[1024];
      int byteCount;
      // Create a loop that reads from the buffered input stream and writes
      // to the zip output stream until the bis has been entirely read.
      while ((byteCount = bis.read(data, 0, 1024)) > -1) {
        zos.write(data, 0, byteCount);
      }

    } catch (IOException e) {
    }

  }

  /**
   * Unzips a file preserving folder info.
   *
   * @param zip
   * @param destFolder
   * @throws IOException
   */
  public static void unzip(File zip, File destFolder) throws IOException {
    unzip(zip, destFolder, true);
  }


  /**
   * @param zip
   * @param destFolder
   * @param useFolderInfo When set to false, prevents directory entries from being extracted.
   */
  public static void unzip(File zip, File destFolder, boolean useFolderInfo) throws IOException {
    FileInputStream zipFIS = new FileInputStream(zip);
    ZipInputStream zis = new ZipInputStream(new BufferedInputStream(zipFIS));
    byte[] data = new byte[1024];
    ZipEntry entry;
    while ((entry = zis.getNextEntry()) != null) {
      String entryName = entry.getName();
      entryName = StringUtilities.replaceAllNoRegex(entryName, new String[]{"/", "\\"}, new String[]{File.separator, File.separator});
      if (entry.isDirectory() && useFolderInfo) {
        File newdir = new File(destFolder, entryName);
        newdir.mkdirs();
      } else if (!entry.isDirectory()) {
        entryName = File.separator + entryName;
        int lastSeparatorPosition = entryName.lastIndexOf(File.separator);
        String dir = entryName.substring(0, lastSeparatorPosition);
        String filename = entryName.substring(lastSeparatorPosition + 1);
        File actualDestFolder;
        if (useFolderInfo)
          actualDestFolder = new File(destFolder, dir);
        else
          actualDestFolder = destFolder;
        actualDestFolder.mkdirs();
        File destFile = new File(actualDestFolder, filename);
        FileOutputStream fos = new FileOutputStream(destFile);
        int count;
        while ((count = zis.read(data, 0, 1024)) != -1) {
          fos.write(data, 0, count);
        }
        fos.flush();
        fos.close();
      }
    }
    zis.close();
  }
}
