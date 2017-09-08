package org.jblooming.flowork;

import org.jbpm.jpdl.xml.JpdlXmlReader;
import org.jbpm.jpdl.xml.Problem;
import org.xml.sax.InputSource;

import java.util.List;
import java.io.Reader;

/**
 * (c) Open Lab - www.open-lab.com
 * Date: Aug 21, 2007
 * Time: 12:21:52 PM
 */
public class FlowUploader extends JpdlXmlReader {

   public FlowUploader(Reader reader) {
    super(reader);
  }

   public List<Problem> getProblems() {
    return problems;
  }
}
