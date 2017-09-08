package com.opnlb.fulltext;

import com.twproject.exchange.msproject.businessLogic.ProjectImportExportControllerAction;
import net.sf.mpxj.ProjectFile;
import net.sf.mpxj.mpp.MPPReader;
import net.sf.mpxj.mpx.MPXReader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.util.PDFTextStripper;
import org.apache.poi.extractor.ExtractorFactory;
import org.jblooming.ontology.PersistentFile;
import org.jblooming.persistence.hibernate.PersistenceContext;
import org.jblooming.tracer.Tracer;
import org.jblooming.utilities.CollectionUtilities;
import org.jblooming.utilities.Zipping;
import org.jblooming.utilities.StringUtilities;
import org.jblooming.utilities.file.FileUtilities;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.w3c.tidy.Tidy;

import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.rtf.RTFEditorKit;
import java.io.*;
import java.util.Set;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipEntry;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Oct 23, 2007
 * Time: 5:48:16 PM
 */
public class TextExtractor {

  public static boolean isOfficeFile(String fileName) {

    Set<String> exts = CollectionUtilities.toSet(".xls", ".xlsx", ".xltx", ".xlsEmb", ".doc", ".docx", ".dotx", ".docEmb", ".ppt", ".pptx", ".msg", ".msgEmb", ".vsd", ".pub");
    return exts.contains(FileUtilities.getFileExt(fileName).toLowerCase());
  }


  public static String getContent(PersistentFile pf) {
    return getContent(pf, PersistenceContext.getDefaultPersistenceContext());
  }

  public static String getContent(PersistentFile pf, PersistenceContext pc) {
    String content = pf.getOriginalFileName();
    InputStream inputStream = null;
    try {

      inputStream = pf.getInputStream(pc);
      String fileName = pf.getOriginalFileName().toLowerCase();
      content = extractFromStream(fileName, inputStream);

    } catch (Throwable e) {
      Tracer.platformLogger.error(e);
    } finally {
      if (inputStream != null)
        try {
          inputStream.close();
        } catch (IOException e) {
        }
    }
    return content;
  }


 
  private static String extractFromStream(String fileName, InputStream inputStream ) throws Exception {

    StringWriter sw = new StringWriter();
    String content = "";

    if (fileName.endsWith(".pdf")) {
      PDFTextStripper stripper = new PDFTextStripper();
      PDDocument document = PDDocument.load(inputStream);
      stripper.writeText(document, sw);
      content = content + sw.getBuffer().toString();
      document.close();

    } else if (fileName.endsWith(".htm") || fileName.endsWith(".html")) {
      Node root = getDOMRoot(inputStream);
      content = getTextContentOfDOM(root);

    } else if (fileName.endsWith(".rtf")) {
      DefaultStyledDocument sd = new DefaultStyledDocument();
      RTFEditorKit kit = new RTFEditorKit();
      kit.read(inputStream, sd, 0);
      content = sd.getText(0, sd.getLength());

    } else if (fileName.endsWith(".zip") || fileName.endsWith(".war") || fileName.endsWith(".jar")) {
      ZipInputStream zis = new ZipInputStream(new BufferedInputStream(inputStream));
      ZipEntry entry;
      while ((entry = zis.getNextEntry()) != null) {
        String entryName = entry.getName();
        entryName = StringUtilities.replaceAllNoRegex(entryName, new String[]{"/", "\\"}, new String[]{File.separator, File.separator});
        if (!entry.isDirectory()) {

          //extract zip files on tmp ones
          File tmpFileOut = File.createTempFile("indexingMachine",".tmp");
          FileOutputStream tmpFos = new FileOutputStream(tmpFileOut);
          int count;
           byte[] data = new byte[1024];
          while ((count = zis.read(data, 0, 1024)) != -1) {
            tmpFos.write(data, 0, count);
          }
          tmpFos.flush();
          tmpFos.close();

          //pass tmp stream to text extractor
          FileInputStream tmpFis= new FileInputStream(tmpFileOut);
          content = content + " " + extractFromStream(entryName, tmpFis);
          tmpFis.close();

          FileUtilities.tryHardToDeleteFile(tmpFileOut);
          
        }
        zis.closeEntry();
      }


    } else if (fileName.endsWith(".log") || fileName.endsWith(".txt")) {
      StringBuffer sb = new StringBuffer();
      BufferedReader br = new BufferedReader(new InputStreamReader(inputStream));
      String line;
      while ((line = br.readLine()) != null) {
        sb.append(line);
        sb.append(" ");
      }
      content = sb.toString();


    } else if (fileName.endsWith(".mpx")) {
      MPXReader mpxReader = new MPXReader();
      ProjectFile mpx = mpxReader.read(inputStream);
      content = ProjectImportExportControllerAction.readProjectData(mpx);

    } else if (fileName.endsWith(".mpp")) {
      MPPReader mppReader = new MPPReader();
      ProjectFile mpp = mppReader.read(inputStream);
      content = ProjectImportExportControllerAction.readProjectData(mpp);


    } else if (isOfficeFile(fileName)) {
      content = ExtractorFactory.createExtractor(inputStream).getText();
    }

    return content;
  }


  private static Node getDOMRoot(InputStream is) {
    Tidy tidy = new Tidy();
    tidy.setQuiet(true);
    tidy.setShowWarnings(false);
    org.w3c.dom.Document doc = tidy.parseDOM(is, null);
    return doc.getDocumentElement();
  }

  private static String getTextContentOfDOM(Node node) {
    NodeList children = node.getChildNodes();
    StringBuffer sb = new StringBuffer();
    for (int i = 0; i < children.getLength(); i++) {
      Node child = children.item(i);
      switch (child.getNodeType()) {
        case Node.ELEMENT_NODE:
          sb.append(getTextContentOfDOM(child));
          sb.append(" ");
          break;
        case Node.TEXT_NODE:
          sb.append(((Text) child).getData());
          break;
      }
    }
    return sb.toString();
  }


}
