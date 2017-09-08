package org.jblooming.logging;

/**
 * Open-lab
 *
 * @author Mauro Manetti  mmanetti@open-lab.com
 */
import org.jblooming.utilities.JSP;

import java.io.*;

public class LogReader {
  //n. di righe ritornate dal tail
  public static String[] tail(String filePath, int n) throws FileNotFoundException {

    //filePath="c:\\tmp";
    File logFile = new File(filePath);
    if(!logFile.exists()){
      return null;
    }
    RandomAccessFile randomLogFile = new RandomAccessFile(logFile,"r");
    int BUFFERSIZE = 1024;
    long pos;
    long endPos;
    long lastPos;
    int numOfLines = 0;
    String info=null;
    byte[] buffer = new byte[BUFFERSIZE];
    StringBuffer sb = new StringBuffer();
    try {
        endPos = randomLogFile.length();
        lastPos = endPos;

        // Check for non-empty file
        // Check for newline at EOF
        if (endPos > 0) {
            byte[] oneByte = new byte[1];
            randomLogFile.seek(endPos - 1);
            randomLogFile.read(oneByte);
            if ((char) oneByte[0] != '\n') {
                numOfLines++;
            }
        }

        do {
            // seek back BUFFERSIZE bytes
            // if length of the file if less then BUFFERSIZE start from BOF
            pos = 0;
            if ((lastPos - BUFFERSIZE) > 0) {
                pos = lastPos - BUFFERSIZE;
            }
            randomLogFile.seek(pos);
            // If less then BUFFERSIZE avaliable read the remaining bytes
            if ((lastPos - pos) < BUFFERSIZE) {
                int remainer = (int) (lastPos - pos);
                buffer = new byte[remainer];
            }
            randomLogFile.readFully(buffer);
            // in the buffer seek back for newlines
            for (int i = buffer.length - 1; i >= 0; i--) {
                if ((char) buffer[i] == '\n') {
                    numOfLines++;
                    // break if we have last n lines
                    if (numOfLines > n) {
                        pos += (i + 1);
                        break;
                    }
                }
            }
            // reset last postion
            lastPos = pos;
        } while ((numOfLines <= n) && (pos != 0));

        // print last n line starting from last postion
        for (pos = lastPos; pos < endPos; pos += buffer.length) {
            randomLogFile.seek(pos);
            if ((endPos - pos) < BUFFERSIZE) {
                int remainer = (int) (endPos - pos);
                buffer = new byte[remainer];
            }
            randomLogFile.readFully(buffer);
            sb.append(new String(buffer));
        }

        info = buildDisplayingHeader(sb.length(), randomLogFile.length());
    } catch (FileNotFoundException e) {
        sb = null;
    } catch (IOException e) {
        e.printStackTrace();
        sb = null;
    } finally {
        try {
            if (randomLogFile != null) {
                randomLogFile.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    if(sb==null){
        return null;
    }

    //String[] tmp = {sb.toString().replaceAll("\\n","<br>"),info};
    String[] tmp = {sb.toString(),info};

    return tmp;

  }


public static String buildDisplayingHeader(int len, long logsize){
         double percent = 0.0;
         if (logsize != 0) {
             percent = ((double) len/logsize) * 100;
         }
         return "Displaying: " + JSP.w(percent) +
             "% of " + JSP.w(formatBytesForDisplay(logsize));

}

   public static String formatBytesForDisplay(long amount) {
         double displayAmount = (double) amount;
         int unitPowerOf1024 = 0;

         if(amount <= 0){
             return "0 B";
         }

         while(displayAmount>=1024 && unitPowerOf1024 < 4) {
             displayAmount = displayAmount / 1024;
             unitPowerOf1024++;
         }

         final String[] units = { " B", " KB", " MB", " GB", " TB" };

         // ensure at least 2 significant digits (#.#) for small displayValues
         //int fractionDigits = (displayAmount < 10) ? 1 : 0;
         return JSP.w(displayAmount)+ units[unitPowerOf1024];
     }


}
