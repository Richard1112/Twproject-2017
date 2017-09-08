package org.jblooming.waf.html.display;

import org.jblooming.utilities.file.FileUtilities;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.DateUtilities;
import org.jblooming.utilities.JSP;
import org.jblooming.PlatformRuntimeException;
import org.jblooming.waf.settings.ApplicationState;

import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.FileOutputStream;
import java.net.URL;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.util.Date;
import java.security.NoSuchAlgorithmException;

/**
 Welcome to Mewsoft Snapshotter
The websites screenshot and thumbnail command line maker tool
Copyrights (c) Mewsoft Corporation 2007. All rights reserved.
Website address: www.mewsoft.com
Support: support@mewsoft.com




 
BEFORE YOU START USING THE PROGRAM, YOU NEED TO REGISTER THE PROGRAM DLLS.





To register the program DLLs, run the file registerdlls.bat once which exists
in the same program folder. You need to run this file once only.

To get the program help, just start the program from the command prompt:

C:\Snapshotter\Snapshotter
 Snapshotter Version 1.0.0. Copyrights (c) 2007 Mewsoft Corporation www.mewsoft.com. All rights reserved.

Usage:
Example single website:

Snapshotter -u "http://www.mewsoft.com" -o "C:\mewsoft.jpg" -w 120 -h 90

Batch URL File:

Snapshotter -l "URLs.txt -o %m  -w 120 -h 90

 Switch       Description:
 ------------------------------------------------------
-u     Website URL
-o     Output filename:
		%m      URL MD5 Hash (Default). Created in the current folder.
		%d      URL Domain name. Created in the current folder.
		%h      URL Hostname. Created in the current folder.
		%f      URL Filename. Created in the current folder.
		other   Creats the filename with entered string in the path specfied.
-p     Output directory. Default is the application directory.
-w     Image width. Default is the full browser width.
-h     Image height. Default is the full browser height.
-bw   Browser width. Default Automatically determined (Set to 0 for default).
-bh   Browser height. Default Automatically determined (Set to 0 for default).
-t     Browser timeout (in milliseconds). Default 40000 ms
-i     Image type extension (allowed values: jpg gif png bmp tiff). Default jpg
-q    JPEG image quality (0 to 100) .Default 85.
-r     Keep thumbnail image aspect ration. Default 1 (0 = disabled, 1 = enabled)
-d    Set image size as browser size, Default 0 (0 = disabled, 1 = enabled)
-f     Force snapshot if time out, Default 0 (0 = disabled, 1 = enabled)
-a    Enable ActiveX, Default 0 (0 = disabled, 1 = enabled)
-j     Enable Java, Default 0 (0 = disabled, 1 = enabled)
-s    Enable Script, Default 0 (0 = disabled, 1 = enabled)
-x    Wait time (milliseconds) after the html document is downloaded (default 1000).
-l     URL list text file, each URL on one line for batch processing.
You can use the switchs with or without the  dash -, for example u or -u or /u are the same.

 */
public class WebThumbnailer {

  public int outputWidth = 400;
  public int outputHeight = 400;
  public int browserWidth = 1024;
  public int browserHeight = 768;

  public int maintainRatio=1;
  public int enableJavaScript=1;
  public int waitTime=1000;

  public int quality=95;

  public String outputName="%%f";
  public String outputFolder="";



  //Snapshotter -u "http://olpc009:8080/html/applications/Licorize/test/testTemplate.jsp" -o "%%f" -s 1 -p C:\tmp\snapshotter\img -w 400 -h 400 -r 1 -bw 1024 -bw 768

  public void generateThumb(String urlToBeThumbnailed, String outputFileName, boolean synchronous) throws IOException, InterruptedException {


    String sh= ApplicationState.webAppFileSystemRootPath+File.separator+"WEB-INF"+File.separator+"lib"+File.separator+"webThumbnailer"+File.separator+"Snapshotter.exe";

    String commandToExecute=sh+" -u \""+urlToBeThumbnailed +" -q "+quality+" -s "+enableJavaScript+ " -o \""+ (JSP.ex(outputFileName)? outputFileName:outputName+" -p "+outputFolder )+"\""+
            " -r "+ maintainRatio+" -w "+outputWidth+" -h "+outputHeight+" -bw "+browserWidth+" -bh "+browserHeight+" -x "+waitTime;

    Process process = Runtime.getRuntime().exec(commandToExecute);

    if (synchronous)
      process.waitFor();

  }

}