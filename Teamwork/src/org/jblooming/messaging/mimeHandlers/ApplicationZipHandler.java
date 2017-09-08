package org.jblooming.messaging.mimeHandlers;

import javax.activation.ActivationDataFlavor;
import javax.activation.DataContentHandler;
import javax.activation.DataSource;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Zip are managed with an array of bytes.
 */
public class ApplicationZipHandler implements DataContentHandler {
  private static ActivationDataFlavor applicationZipDF = new ActivationDataFlavor(byte[].class, "application/zip", "ZIP archive");

  public Object getContent(DataSource dataSource) throws IOException {
    InputStream is = dataSource.getInputStream();
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    int b;
    while ((b = is.read()) != -1) {
      baos.write(b);
    }
    return baos.toByteArray();
  }

  public Object getTransferData(DataFlavor dataFlavor, DataSource dataSource) throws UnsupportedFlavorException, IOException {
    if (applicationZipDF.equals(dataFlavor))
      return getContent(dataSource);
    else
      return null;
  }

  public DataFlavor[] getTransferDataFlavors() {
    return new DataFlavor[]{applicationZipDF};
  }

  public void writeTo(Object content, String mimeType, OutputStream outputStream) throws IOException {
    if (!(content instanceof byte[]))
      throw new IOException("\"" + applicationZipDF.getMimeType() + "\" DataContentHandler requires byte[] object, " +
              "was given object of type " + content.getClass().toString());
    outputStream.write((byte[]) content);
  }
}
