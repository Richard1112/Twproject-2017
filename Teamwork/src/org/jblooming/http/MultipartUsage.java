package org.jblooming.http;

import javax.servlet.http.HttpServletRequest;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;


public class MultipartUsage {

  protected void main(HttpServletRequest request) throws IOException {
    byte[] content = null;

    String mimetype = "";

    String originalFileName = "";

    String formFileFieldName = "attachmentData";

    String serverFileName = request.getParameter(formFileFieldName);


    if (serverFileName != null) {
      originalFileName = request.getParameter(formFileFieldName + ".filename");
      mimetype = request.getParameter(formFileFieldName + ".content-type");
      File file = new File(serverFileName);
      if (file.exists()) {
        FileInputStream input = new FileInputStream(file);
        int byteCount = input.available();
        byte[] data = new byte[byteCount];
        input.read(data);
        content = data;
        input.close();
      }
    }

    try {
      int startIdx = originalFileName.lastIndexOf(File.separatorChar);
      originalFileName = originalFileName.substring(startIdx + 1);
    } catch (Throwable t) {
    }
  }

}
